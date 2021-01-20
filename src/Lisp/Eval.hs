{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

module Lisp.Eval
  ( eval
  , primProcs
  ) where

import           Control.Applicative            ( liftA2 )
import           Control.Monad.Except           ( liftIO
                                                , throwError
                                                )
import           Data.Bool                      ( bool )
import           Data.Either                    ( isLeft )
import           Data.IORef                     ( IORef )
import qualified Data.List.NonEmpty            as NE
import           Lisp.Parser
import           Lisp.Scope
import           Lisp.Types              hiding ( body
                                                , closure
                                                , params
                                                )
import           Parser
import           Prelude                 hiding ( init )
import           Util

type Unpacker a =
  LispVal -> ThrowsException a

type IOUnpacker a =
  LispVal -> IOThrowsException a

primProcs :: [(Keyword, PrimProc)]
primProcs =
  [ (KAnd   , foldOp (&&)  unpackBool  Bool)
  , (KOr    , foldOp (||)  unpackBool  Bool)
  , (KPlus  , foldOp (+)   unpackNum   Number)
  , (KMinus , foldOp (-)   unpackNum   Number)
  , (KMult  , foldOp (*)   unpackNum   Number)
  , (KDiv   , foldOp (/)   unpackNum   Number)
  , (KMod   , binOp  mod'' unpackNum   Number)
  , (KQuot  , binOp  quot  unpackExact (Number . Exact))
  , (KRem   , binOp  rem   unpackExact (Number . Exact))
  , (KEq    , binOp  (==)  unpackNum   Bool)
  , (KLT    , binOp  (<)   unpackNum   Bool)
  , (KGT    , binOp  (>)   unpackNum   Bool)
  , (KLTE   , binOp  (<=)  unpackNum   Bool)
  , (KGTE   , binOp  (>=)  unpackNum   Bool)
  , (KStrEq , binOp  (==)  unpackStr   Bool)
  , (KStrLT , binOp  (<)   unpackStr   Bool)
  , (KStrGT , binOp  (>)   unpackStr   Bool)
  , (KStrLTE, binOp  (<=)  unpackStr   Bool)
  , (KStrGTE, binOp  (>=)  unpackStr   Bool)
  ]

-- ---------------------------------------------------------------------------
-- Evaluation

eval :: IORef Scope -> LispVal -> IOThrowsException LispVal
eval _   n@(Number    _      ) = return n
eval _   c@(Character _      ) = return c
eval _   s@(String    _      ) = return s
eval _   b@(Bool      _      ) = return b
eval ref (  Atom      ident  ) = getVar ref (either id show ident)
eval ref p@(Pair _ (List _  )) = eval ref $ flatten p
eval ref p@(Pair _ (Pair _ _)) = eval ref $ flatten p
eval ref (  List (h : args)  ) = case h of
  Atom (Right KQuote ) -> quoteExpr ref args
  Atom (Right KIf    ) -> ifStatement ref args
  Atom (Right KSet   ) -> setExpr ref args
  Atom (Right KDefine) -> defineExpr ref args
  Atom (Right KLambda) -> lambdaExpr ref args
  Atom (Right KLet)    -> letExpr ref args
  _                    -> do
    func    <- eval ref h
    argVals <- mapM (eval ref) args
    apply func argVals
eval _ (List []) = return $ List []
eval _ val       = throwError $ BadExpression val

apply :: LispVal -> [LispVal] -> IOThrowsException LispVal
apply (PrimProc proc    ) args = liftThrow $ proc args
apply (Proc     procBody) args = applyProcedure procBody args
apply val                      _    = throwError $ CannotApply val

applyProcedure :: ProcBody -> [LispVal] -> IOThrowsException LispVal
applyProcedure (ProcBody scope params body) args = do
  localScope <- getLocalScope params args
  closure    <- liftIO $ makeClosure scope localScope
  NE.last <$> traverse (eval closure) body

getLocalScope ::
  ([String], Maybe String)
  -> [LispVal]
  -> IOThrowsException [(String, LispVal)]
getLocalScope (params, arity) args =
  let n = length params
   in case arity of
        Nothing       -> if n /= length args
          then throwError $ InvalidArgs (Exactly n) args
          else return $ zip params args
        Just arityArg -> if n > length args
          then throwError $ InvalidArgs (AtLeast n) args
          else return $ (arityArg, List $ drop n args) : zip params args

-- ---------------------------------------------------------------------------
-- Expressions

quoteExpr :: IORef Scope -> [LispVal] -> IOThrowsException LispVal
quoteExpr _   [val] = return $ flatten val
quoteExpr _   args  = throwError $ InvalidArgs (Exactly 1) args

ifStatement :: IORef Scope -> [LispVal] -> IOThrowsException LispVal
ifStatement ref [p, onT, onF] =
  eval ref p >>= (eval ref . bool onT onF . isFalse)
ifStatement _ args = throwError $ InvalidArgs (Exactly 3) args

letExpr :: IORef Scope -> [LispVal] -> IOThrowsException LispVal
letExpr ref (bindings : body) = case NE.nonEmpty body of
  Nothing    -> throwError EmptyBody
  Just body' -> do
    bs        <- liftThrow (unpackList unpackBinding bindings)
                   >>= traverse (secondM $ eval ref)
    ensureUnique $ fst <$> bs
    closure   <- liftIO $ makeClosure ref bs
    NE.last <$> traverse (eval closure) body'
letExpr _ args  = throwError $ InvalidArgs (Exactly 2) args

unpackBinding :: LispVal -> ThrowsException (String, LispVal)
unpackBinding (List [ident, init]) = unpackId ident >>= (return . (, init))
unpackBinding val                  = throwError $ TypeMismatch "binding" val

unpackList :: Unpacker a -> Unpacker [a]
unpackList g p@(Pair _ (List _  )) = unpackList g $ flatten p
unpackList g p@(Pair _ (Pair _ _)) = unpackList g $ flatten p
unpackList g (List vals)           = traverse g vals
unpackList _ val                   = throwError $ TypeMismatch "list" val

setExpr :: IORef Scope -> [LispVal] -> IOThrowsException LispVal
setExpr ref [ident, val] =
  bind2 (setVar ref) (liftThrow $ unpackId ident) (eval ref val)
setExpr _ args = throwError $ InvalidArgs (Exactly 2) args

defineExpr :: IORef Scope -> [LispVal] -> IOThrowsException LispVal
defineExpr ref [i@(Atom _), val] =
  bind2 (defineVar ref) (ioUnpackId i) (eval ref val)
defineExpr ref (List (i : params) : body) =
  bind2 (defineVar ref) (ioUnpackId i) (makeProc ref (params, Nothing) body)
defineExpr ref (Pair (i : params) arity : body) =
  bind2 (defineVar ref) (ioUnpackId i) (makeProc ref (params, Just arity) body)
defineExpr _ args =
  throwError $ BadExpression $ List $ Atom (Right KDefine) : args

lambdaExpr :: IORef Scope -> [LispVal] -> IOThrowsException LispVal
lambdaExpr ref (List params : body) =
  makeProc ref (params, Nothing) body
lambdaExpr ref (Pair params arity : body) =
  makeProc ref (params, Just arity) body
lambdaExpr ref (arity : body) =
  makeProc ref ([], Just arity) body
lambdaExpr _ args =
  throwError $ BadExpression $ List $ Atom (Right KLambda) : args

-- ---------------------------------------------------------------------------
-- Unpackers

unpackNum :: Unpacker LispNumber
unpackNum (Number n) = return n
unpackNum val        = throwError $ TypeMismatch "number" val

unpackExact :: Unpacker Integer
unpackExact val =
  unpackNum val
    >>= (\case
          Exact e       -> return e
          i@(Inexact _) -> throwError $ TypeMismatch "exact number" (Number i)
        )

unpackStr :: Unpacker String
unpackStr (String s) = return s
unpackStr val        = throwError $ TypeMismatch "string" val

unpackBool :: Unpacker Bool
unpackBool (Bool b) = return b
unpackBool val      = throwError $ TypeMismatch "boolean" val

unpackId :: Unpacker String
unpackId (Atom (Left str)) = if isLeft $ parse lispIdentifier str
  then throwError $ InvalidIdentifier str
  else return str
unpackId (Atom (Right k))  = throwError $ VarNameReserved $ show k
unpackId invalid           = throwError $ InvalidIdentifier $ show invalid

ioUnpackId :: IOUnpacker String
ioUnpackId = liftThrow . unpackId

-- ---------------------------------------------------------------------------
-- Helpers

flatten :: LispVal -> LispVal
flatten (Pair h (  List t  )) = List (map flatten h ++ map flatten t)
flatten (Pair h t@(Pair _ _)) = case flatten t of
  List l     -> List (map flatten h ++ l)
  Pair h' t' -> Pair (map flatten h ++ h') t'
  val        -> Pair (map flatten h) val
flatten a = a

makeProc ::
  IORef Scope
  -> ([LispVal], Maybe LispVal)
  -> [LispVal]
  -> IOThrowsException LispVal
makeProc scope (params, arity) body = case NE.nonEmpty body of
  Nothing    -> throwError EmptyBody
  Just body' -> do
    ps <- traverse ioUnpackId params
    a  <- mapM ioUnpackId arity
    ensureUnique $ maybe ps (: ps) a
    return $ Proc $ ProcBody scope (ps, a) body'

ensureUnique :: [String] -> IOThrowsException ()
ensureUnique as =
  case dup as of
    Just arg -> throwError (NonUniqueBinding arg)
    Nothing  -> return ()

unOp ::
  (a -> b)
  -> (LispVal -> ThrowsException a)
  -> (b -> LispVal)
  -> [LispVal]
  -> ThrowsException LispVal
unOp f unpack tc args = case args of
  [x] -> tc . f <$> unpack x
  _   -> throwError $ InvalidArgs (Exactly 1) args

binOp ::
  (a -> a -> b)
  -> (LispVal -> ThrowsException a)
  -> (b -> LispVal)
  -> [LispVal]
  -> ThrowsException LispVal
binOp f unpack tc args = case args of
  [x, y] -> tc <$> liftA2 f (unpack x) (unpack y)
  _      -> throwError $ InvalidArgs (Exactly 2) args

foldOp ::
  (a -> a -> a)
  -> (LispVal -> ThrowsException a)
  -> (a -> LispVal)
  -> [LispVal]
  -> ThrowsException LispVal
foldOp f unpack tc args =
  if length args >= 2
    then tc . foldl1 f <$> traverse unpack args
    else throwError $ InvalidArgs (AtLeast 2) args

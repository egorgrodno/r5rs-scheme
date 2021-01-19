{-# LANGUAGE LambdaCase #-}

module Lisp.Eval
  ( eval
  , primProcs
  ) where

import           Control.Applicative            ( liftA2 )
import           Control.Monad                  ( liftM
                                                , liftM2
                                                )
import           Control.Monad.Except           ( liftIO
                                                , throwError
                                                , runExceptT
                                                )
import           Data.Bool                      ( bool )
import           Data.Either                    ( isLeft )
import           Data.IORef                     ( IORef )
import qualified Data.List.NonEmpty            as NE
import           Lisp.Parser
import           Lisp.Scope
import           Lisp.Types
import           Parser
import           Util

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
  _             -> do
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
quoteExpr ref [val] = return $ flatten val
quoteExpr _   args  = throwError $ InvalidArgs (Exactly 1) args

ifStatement :: IORef Scope -> [LispVal] -> IOThrowsException LispVal
ifStatement ref [p, onT, onF] =
  eval ref p >>= (eval ref . bool onT onF . isFalse)
ifStatement _   args          = throwError $ InvalidArgs (Exactly 3) args

setExpr :: IORef Scope -> [LispVal] -> IOThrowsException LispVal
setExpr ref [ident, val] =
  bind2 (setVar ref) (unpackId ident) (eval ref val)
setExpr _   args  = throwError $ InvalidArgs (Exactly 2) args

defineExpr :: IORef Scope -> [LispVal] -> IOThrowsException LispVal
defineExpr ref [i@(Atom _), val] =
  bind2 (defineVar ref) (unpackId i) (eval ref val)
defineExpr ref (List (i : params) : body) =
  bind2 (defineVar ref) (unpackId i) (makeProc ref (params, Nothing) body)
defineExpr ref (Pair (i : params) arity : body) =
  bind2 (defineVar ref) (unpackId i) (makeProc ref (params, Just arity) body)
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

unpackNum :: LispVal -> ThrowsException LispNumber
unpackNum (Number n) = return n
unpackNum val        = throwError $ TypeMismatch "number" val

unpackExact :: LispVal -> ThrowsException Integer
unpackExact val =
  unpackNum val
    >>= (\case
          Exact e       -> return e
          i@(Inexact _) -> throwError $ TypeMismatch "exact number" (Number i)
        )

unpackStr :: LispVal -> ThrowsException String
unpackStr (String s) = return s
unpackStr val        = throwError $ TypeMismatch "string" val

unpackBool :: LispVal -> ThrowsException Bool
unpackBool (Bool b) = return b
unpackBool val      = throwError $ TypeMismatch "boolean" val

unpackArity :: Maybe LispVal -> IOThrowsException (Maybe String)
unpackArity Nothing      = return Nothing
unpackArity (Just arity) = Just <$> unpackId arity

unpackId :: LispVal -> IOThrowsException String
unpackId (Atom (Left str)) = if isLeft $ parse lispIdentifier str
  then throwError $ InvalidIdentifier str
  else return str
unpackId (Atom (Right k))  = throwError $ VarNameReserved $ show k
unpackId invalid           = throwError $ InvalidIdentifier $ show invalid

unpackIds :: [LispVal] -> IOThrowsException [String]
unpackIds = traverse unpackId

-- ---------------------------------------------------------------------------
-- Helpers

flatten :: LispVal -> LispVal
flatten (Pair h (List t)) =
  List (map flatten h ++ map flatten t)
flatten (Pair h t@(Pair h2 t2)) = case flatten t of
  List t     -> List (map flatten h ++ t)
  Pair h2 t2 -> Pair (map flatten h ++ h2) t2
  t          -> Pair (map flatten h) t
flatten a = a

makeProc ::
  IORef Scope
  -> ([LispVal], Maybe LispVal)
  -> [LispVal]
  -> IOThrowsException LispVal
makeProc scope (params, arity) body = case NE.nonEmpty body of
  Nothing    -> throwError EmptyBody
  Just body' -> do
    paramsT <- bind2 ensureUnique (unpackIds params) (unpackArity arity)
    return $ Proc $ ProcBody scope paramsT body'

ensureUnique ::
  [String]
  -> Maybe String
  -> IOThrowsException ([String], Maybe String)
ensureUnique params arity =
  case dup $ maybe params (: params) arity of
    Just arg -> throwError (NonUniqueBinding arg)
    Nothing  -> return (params, arity)

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

{-# LANGUAGE LambdaCase #-}

module Lisp.Eval
  ( eval
  ) where

import           Control.Applicative            ( liftA2 )
import           Control.Monad.Except           ( throwError )
import           Lisp.Prim                      ( LispNumber(..)
                                                , LispVal(..)
                                                , mod''
                                                , showListVals
                                                )


type LispFunction =
  [LispVal] -> ThrowsException LispVal

type ThrowsException =
  Either LispException

data LispException =
  InvalidArgs NArgs [LispVal]
  | TypeMismatch String LispVal
  | NotAFunction String
  | CannotEval LispVal

instance Show LispException where
  show = showException

data NArgs =
  Exactly Int
  | AtLeast Int

instance Show NArgs where
  show = showNArgs

showException :: LispException -> String
showException (InvalidArgs n got) =
  let msg = "Wrong number of args given, expected " ++ show n ++ ", got "
   in case got of
        [] -> msg ++ "0"
        _  -> msg ++ show (length got) ++ " (" ++ showListVals got ++ ")"
showException (TypeMismatch t got) =
  "Wrong type, expected " ++ t ++ ", got " ++ show got
showException (NotAFunction name) =
  "Function " ++ name ++ " not recognized"
showException (CannotEval expr) =
  "Cannot eval " ++ show expr

showNArgs :: NArgs -> String
showNArgs (Exactly n) = show n
showNArgs (AtLeast n) = "at least " ++ show n

eval :: LispVal -> ThrowsException LispVal
eval n@(Number    _                  ) = return n
eval c@(Character _                  ) = return c
eval s@(String    _                  ) = return s
eval b@(Bool      _                  ) = return b
eval (  List      [Atom "quote", val]) = return val
eval (  List      (Atom fname : args)) = traverse eval args >>= apply fname
eval val                               = throwError $ CannotEval val

apply :: String -> [LispVal] -> ThrowsException LispVal
apply name args =
  maybe (throwError $ NotAFunction name) ($ args) $ lookup name functions

functions :: [(String, LispFunction)]
functions =
  [ ("and"      , foldOp (&&)  unpackBool  Bool)
  , ("or"       , foldOp (||)  unpackBool  Bool)
  , ("+"        , foldOp (+)   unpackNum   Number)
  , ("-"        , foldOp (-)   unpackNum   Number)
  , ("*"        , foldOp (*)   unpackNum   Number)
  , ("/"        , foldOp (/)   unpackNum   Number)
  , ("modulo"   , binOp  mod'' unpackNum   Number)
  , ("quotient" , binOp  quot  unpackExact (Number . Exact))
  , ("remainder", binOp  rem   unpackExact (Number . Exact))
  , ("="        , binOp  (==)  unpackNum   Bool)
  , ("<"        , binOp  (<)   unpackNum   Bool)
  , (">"        , binOp  (>)   unpackNum   Bool)
  , (">="       , binOp  (>=)  unpackNum   Bool)
  , ("<="       , binOp  (<=)  unpackNum   Bool)
  , ("string=?" , binOp  (==)  unpackStr   Bool)
  , ("string<?" , binOp  (<)   unpackStr   Bool)
  , ("string>?" , binOp  (>)   unpackStr   Bool)
  , ("string<=?", binOp  (<=)  unpackStr   Bool)
  , ("string>=?", binOp  (>=)  unpackStr   Bool)
  ]

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

{-# LANGUAGE LambdaCase #-}

module Lisp.Eval
  ( eval
  ) where

import           Control.Applicative            ( liftA2 )
import           Control.Monad.Except           ( throwError )
import           Data.Bool                      ( bool )
import           Data.IORef                     ( IORef )
import           Lisp.Except
import           Lisp.Prim
import           Lisp.Scope

type LispFunction =
  [LispVal] -> ThrowsException LispVal

eval :: IORef Scope -> LispVal -> IOThrowsException LispVal
eval _   n@(Number    _                              ) = return n
eval _   c@(Character _                              ) = return c
eval _   s@(String    _                              ) = return s
eval _   b@(Bool      _                              ) = return b
eval ref (  Atom      id                             ) = getVar ref id
eval _   (  List      [Atom "quote", val]            ) = return val
eval ref (  List      [Atom "if", p, onTrue, onFalse]) = eval ref p >>= (eval ref . bool onTrue onFalse . (== Bool False))
eval ref (  List      [Atom "set!", Atom name, val]  ) = eval ref val >>= setVar ref name
eval ref (  List      [Atom "define", Atom name, val]) = eval ref val >>= defineVar ref name
eval ref (  List      (Atom fname : args)            ) = traverse (eval ref) args >>= (liftThrow . apply fname)
eval _   val                                           = throwError $ CannotEval val

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

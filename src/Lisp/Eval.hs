module Lisp.Eval
  ( eval
  ) where

import           Lisp.Prim                      ( LispNumber(..)
                                                , LispVal(..)
                                                , showListVals
                                                )
import           Control.Monad.Except


type LispFunction =
  [LispVal] -> ThrowsException LispVal

type ThrowsException =
  Either LispException

data LispException =
  InvalidArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | NotAFunction String
  | CannotEval LispVal

instance Show LispException where
  show = showException

showException :: LispException -> String
showException (InvalidArgs n got) =
  "Expected " ++ show n ++ " args, got " ++ showListVals got
showException (TypeMismatch t got) =
  "Expected " ++ t ++ " type, got " ++ show got
showException (NotAFunction name) =
  "Unknown function " ++ name
showException (CannotEval expr) =
  "Cannot eval " ++ show expr

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
  [ ("+", foldl2Nums (+))
  , ("-", foldl2Nums (-))
  , ("*", foldl2Nums (*))
  , ("/", foldl2Nums (/))
  ]

foldl2Nums ::
  (LispNumber -> LispNumber -> LispNumber)
  -> [LispVal]
  -> ThrowsException LispVal
foldl2Nums _ []   = throwError $ InvalidArgs 2 []
foldl2Nums _ [v]  = throwError $ InvalidArgs 2 [v]
foldl2Nums f args = Number . foldl1 f <$> traverse unpackNum args

unpackNum :: LispVal -> ThrowsException LispNumber
unpackNum (Number n  ) = return n
unpackNum (List   [n]) = unpackNum n
unpackNum val          = throwError $ TypeMismatch "number" val

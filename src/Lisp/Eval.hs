{-# LANGUAGE LambdaCase #-}

module Lisp.Eval
  ( eval
  ) where

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
  [ ("+"        , makeFunc (+)   unpackNum   (AtLeast 2) Number)
  , ("-"        , makeFunc (-)   unpackNum   (AtLeast 2) Number)
  , ("*"        , makeFunc (*)   unpackNum   (AtLeast 2) Number)
  , ("/"        , makeFunc (/)   unpackNum   (AtLeast 2) Number)
  , ("mod"      , makeFunc mod'' unpackNum   (Exactly 2) Number)
  , ("quotient" , makeFunc quot  unpackExact (Exactly 2) (Number . Exact))
  , ("remainder", makeFunc rem   unpackExact (Exactly 2) (Number . Exact))
  ]

satisfies :: NArgs -> Int -> Bool
satisfies (Exactly n) nArgs = nArgs == n
satisfies (AtLeast n) nArgs = nArgs >= n

makeFunc ::
  (a -> a -> a)
  -> (LispVal -> ThrowsException a)
  -> NArgs
  -> (a -> LispVal)
  -> [LispVal]
  -> ThrowsException LispVal
makeFunc f unpack n tc args =
  if satisfies n (length args)
    then tc . foldl1 f <$> traverse unpack args
    else throwError $ InvalidArgs n args

unpackNum :: LispVal -> ThrowsException LispNumber
unpackNum (Number n  ) = return n
unpackNum (List   [n]) = unpackNum n
unpackNum val          = throwError $ TypeMismatch "number" val

unpackExact :: LispVal -> ThrowsException Integer
unpackExact val =
  unpackNum val
    >>= (\case
          Exact e       -> return e
          i@(Inexact _) -> throwError $ TypeMismatch "exact number" (Number i)
        )

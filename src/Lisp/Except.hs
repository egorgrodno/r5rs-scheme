module Lisp.Except
  ( IOThrowsException
  , LispException(..)
  , NArgs(..)
  , ThrowsException
  , liftThrow
  ) where

import           Control.Monad.Except           ( ExceptT
                                                , throwError
                                                )
import           Lisp.Prim                      ( LispVal
                                                , showListVals
                                                )

type ThrowsException =
  Either LispException

type IOThrowsException =
  ExceptT LispException IO

data LispException =
  InvalidArgs NArgs [LispVal]
  | TypeMismatch String LispVal
  | NotAFunction String
  | CannotEval LispVal
  | GettingUnboundVar String
  | SettingUnboundVar String

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
showException (GettingUnboundVar name) =
  "Getting unbound var " ++ name
showException (SettingUnboundVar name) =
  "Setting unbound var " ++ name

showNArgs :: NArgs -> String
showNArgs (Exactly n) = show n
showNArgs (AtLeast n) = "at least " ++ show n

liftThrow :: ThrowsException a -> IOThrowsException a
liftThrow (Left  err) = throwError err
liftThrow (Right val) = return val

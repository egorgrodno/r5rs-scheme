module Except where

import           Control.Monad.Except           ( ExceptT
                                                , throwError
                                                )
import           Lisp.Types                     ( LispException )
import           Parser.Except                  ( ParseException )

data AppException =
  ParseException ParseException
  | LispException LispException

instance Show AppException where
  show = showAppException

type Throws e =
  Either e

type IOThrows e =
  ExceptT e IO

liftThrow :: Throws e a -> IOThrows e a
liftThrow (Left  err) = throwError err
liftThrow (Right val) = return val

showAppException :: AppException -> String
showAppException (ParseException e) = show e
showAppException (LispException  e) = show e

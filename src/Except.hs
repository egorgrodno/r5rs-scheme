module Except where

import           Control.Monad.Except           ( ExceptT
                                                , throwError
                                                )
import           Lisp.Except                    ( LispException )
import           Parser.Except                  ( ParseException )

data AppException =
  InvalidCliArgs String
  | ParseException ParseException
  | LispException LispException

type Throws e =
  Either e

type IOThrows e =
  ExceptT e IO

liftThrow :: Throws e a -> IOThrows e a
liftThrow (Left  err) = throwError err
liftThrow (Right val) = return val

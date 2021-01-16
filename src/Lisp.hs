module Lisp
  ( eval
  , eval'
  , lispExpr
  , nullScope
  ) where

import           Control.Monad.Except           ( withExceptT )
import           Data.IORef                     ( IORef )
import           Except
import           Lisp.Eval
import           Lisp.Parser
import           Lisp.Prim
import           Lisp.Scope

eval' :: IORef Scope -> LispVal -> IOThrows AppException LispVal
eval' ref val = withExceptT LispException (eval ref val)

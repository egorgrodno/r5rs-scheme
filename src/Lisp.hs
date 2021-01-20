module Lisp
  ( LispVal(..)
  , Scope
  , baseScope
  , eval
  , eval'
  , lispExpr
  ) where

import           Control.Monad.Except           ( withExceptT )
import           Data.Bifunctor                 ( bimap )
import           Data.IORef                     ( IORef )
import           Except
import           Lisp.Eval
import           Lisp.Parser
import           Lisp.Scope
import           Lisp.Types
import           Prelude

baseScope :: IO (IORef Scope)
baseScope = do
  scope <- nullScope
  makeClosure scope (bimap show PrimProc <$> primProcs)

eval' :: IORef Scope -> LispVal -> IOThrows AppException LispVal
eval' ref val = withExceptT LispException (eval ref val)

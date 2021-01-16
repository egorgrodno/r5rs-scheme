{-# LANGUAGE TupleSections #-}

module Lisp.Scope where

import           Control.Monad.Except           ( liftIO
                                                , throwError
                                                )
import           Data.IORef                     ( IORef
                                                , newIORef
                                                , readIORef
                                                , writeIORef
                                                )
import           Data.Maybe                     ( isJust )
import           Lisp.Except
import           Lisp.Prim

type Scope =
  [(String, IORef LispVal)]

nullScope :: IO (IORef Scope)
nullScope = newIORef []

readIORef' :: IORef a -> IOThrowsException a
readIORef' = liftIO . readIORef

writeIORef' :: IORef a -> a -> IOThrowsException a
writeIORef' ref a = liftIO $ writeIORef ref a >> return a

isDefined :: IORef Scope -> String -> IOThrowsException Bool
isDefined ref var = liftIO $ isJust . lookup var <$> readIORef ref

getVar :: IORef Scope -> String -> IOThrowsException LispVal
getVar ref name = do
  env <- readIORef' ref
  maybe (throwError $ GettingUnboundVar name) readIORef' (lookup name env)

setVar :: IORef Scope -> String -> LispVal -> IOThrowsException LispVal
setVar ref name val = do
  env <- readIORef' ref
  maybe (throwError $ SettingUnboundVar name)
        (`writeIORef'` val)
        (lookup name env)

defineVar :: IORef Scope -> String -> LispVal -> IOThrowsException LispVal
defineVar ref name val = do
  defined <- isDefined ref name
  if defined
    then setVar ref name val
    else liftIO $ do
      valRef <- newIORef val
      env <- readIORef ref
      writeIORef ref ((name, valRef) : env)
      return val

makeClosure :: IORef Scope -> [(String, LispVal)] -> IO (IORef Scope)
makeClosure ref bindings =
  let makeBinding (name, val) = (name,) <$> newIORef val
   in do
        scope <- readIORef ref
        newScope <- traverse makeBinding bindings
        newIORef (newScope ++ scope)


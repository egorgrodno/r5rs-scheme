{-# LANGUAGE LambdaCase #-}

module Main
  ( main
  ) where

import           Control.Monad.Except           ( runExceptT
                                                , throwError
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Lisp                           ( eval'
                                                , lispExpr
                                                , nullScope
                                                )
import           Parser                         ( eof
                                                , parse'
                                                , spaces
                                                )
import           System.Environment             ( getArgs )
import           System.Exit                    ( ExitCode(..)
                                                , exitSuccess
                                                , exitWith
                                                )
import           Except                         ( AppException(..)
                                                , IOThrows
                                                )

main :: IO ()
main =
  let parseAndEvalLisp str = do
        scopeRef <- liftIO nullScope
        (_, val) <- parse' (lispExpr <* spaces <* eof) str
        eval' scopeRef val
   in do
        result <- runExceptT (parseArgs >>= parseAndEvalLisp)
        case result of
          Left  (InvalidCliArgs  str) -> putStrLn str >> usage >> die
          Left  (ParseException  err) -> print err >> die
          Left  (LispException   err) -> print err >> die
          Right val                   -> print val >> exitSuccess

parseArgs :: IOThrows AppException String
parseArgs =
  liftIO getArgs
    >>= (\case
          [a] -> return a
          [ ] -> throwError $ InvalidCliArgs "Not enough arguments"
          _   -> throwError $ InvalidCliArgs "Too many arguments"
        )

usage :: IO ()
usage = putStrLn "Usage: r5rs-scheme STRING"

die :: IO ()
die = exitWith (ExitFailure 1)

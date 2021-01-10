{-# LANGUAGE LambdaCase #-}

module Main where

import           Lisp                           ( lispExpr )
import           Parser                         ( parse )
import           System.Environment             ( getArgs )
import           System.Exit                    ( ExitCode(..)
                                                , exitSuccess
                                                , exitWith
                                                )


main :: IO ()
main =
  parseArgs <$> getArgs
    >>= (\case
          Left err ->
            putStrLn ("Error parsing arguments: " ++ err) >> usage >> die
          Right str ->
            print (parse lispExpr str) >> exitSuccess
        )

parseArgs :: [String] -> Either String String
parseArgs (_ : _ : _) =
  Left "Too many arguments"
parseArgs [a] =
  Right a
parseArgs [] =
  Left "Not enough arguments"

usage :: IO ()
usage =
  putStrLn "Usage: r5rs-scheme STRING"

die :: IO ()
die =
  exitWith (ExitFailure 1)

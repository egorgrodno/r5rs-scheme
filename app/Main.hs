{-# LANGUAGE LambdaCase #-}

module Main
  ( main
  ) where

import           Data.Bifunctor                 ( bimap )
import           Lisp                           ( eval
                                                , lispExpr
                                                )
import           Parser                         ( Parser
                                                , ParseResult(..)
                                                , eof
                                                , parse
                                                , spaces
                                                )
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
          Right str -> case parseAndEval str of
            Left  err    -> putStrLn err >> die
            Right result -> putStrLn result >> exitSuccess
        )

parseAndEval :: String -> Either String String
parseAndEval str =
  parseToEither (lispExpr <* spaces <* eof) str >>= bimap show show . eval

parseToEither :: Show a => Parser a -> String -> Either String a
parseToEither p i = case parse p i of
  Result _ expr -> Right expr
  err           -> Left $ show err

parseArgs :: [String] -> Either String String
parseArgs (_ : _ : _) = Left "too many arguments"
parseArgs [a        ] = Right a
parseArgs []          = Left "not enough arguments"

usage :: IO ()
usage = putStrLn "Usage: r5rs-scheme STRING"

die :: IO ()
die = exitWith (ExitFailure 1)

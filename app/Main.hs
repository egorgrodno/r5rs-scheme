{-# LANGUAGE LambdaCase #-}

module Main
  ( main
  ) where

import           Control.Monad                  ( unless )
import           Control.Monad.Except           ( runExceptT
                                                , throwError
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.IORef                     ( IORef )
import           Data.Version                   ( showVersion )
import           Except                         ( AppException(..)
                                                , IOThrows
                                                )
import           Lisp                           ( LispVal
                                                , Scope
                                                , eval'
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
import           System.IO                      ( hFlush
                                                , stdout
                                                )
import qualified Paths_r5rs_scheme             as P

type Script =
  String

data Action =
  RunRepl
  | Eval Script
  | PrintHelp
  | PrintVersion

main :: IO ()
main = do
  args <- runExceptT parseArgs
  case args of
    Left err            -> putStrLn err >> usage >> die
    Right PrintHelp     -> help >> exitSuccess
    Right PrintVersion  -> version >> exitSuccess
    Right RunRepl       -> startRepl >> exitSuccess
    Right (Eval script) -> do
      result <- runExceptT $ runScript script
      case result of
        Left err  -> print err >> die
        Right val -> print val >> exitSuccess

parseArgs :: IOThrows String Action
parseArgs =
  liftIO getArgs
    >>= (\case
          []                 -> return RunRepl
          ["-h"       ]      -> return PrintHelp
          ["--help"   ]      -> return PrintHelp
          ["-v"       ]      -> return PrintVersion
          ["--version"]      -> return PrintVersion
          ["-e"    , script] -> return $ Eval script
          ["--eval", script] -> return $ Eval script
          _                  -> throwError "Bad arguments"
        )

runScript :: String -> IOThrows AppException LispVal
runScript str = do
  scopeRef <- liftIO nullScope
  parseAndEval scopeRef str

startRepl :: IO ()
startRepl =
  let loop ref = do
        input <- prompt
        unless (input == "quit") (evalAndPrint ref input >> loop ref)
   in nullScope >>= loop

evalAndPrint :: IORef Scope -> String -> IO ()
evalAndPrint ref str =
  either show show <$> runExceptT (parseAndEval ref str) >>= putStrLn

parseAndEval :: IORef Scope -> String -> IOThrows AppException LispVal
parseAndEval ref str =
  parse' (lispExpr <* spaces <* eof) str >>= (eval' ref . snd)

prompt :: IO String
prompt = putStr "lisp>>> " >> hFlush stdout >> getLine

version :: IO ()
version = putStrLn $ "r5rs-scheme v" ++ showVersion P.version

usage :: IO ()
usage =
  putStrLn "Usage: r5rs-scheme [--eval, -e SCRIPT] [--version, -v] [--help, -h]"

help :: IO ()
help =
  let opts =
        [ ("-",                "Interactive mode (if no arguments is provided)")
        , ("--help,-h",        "Show this help text")
        , ("--version,-v",     "Show version")
        , ("--eval,-e SCRIPT", "Evaluate script")
        ]
      spacer n = replicate n ' '
      showOption (name, desc) =
        spacer 2 ++ name ++ spacer (23 - length name) ++ desc
   in do
        putStrLn "r5rs-scheme - R5RS Scheme Interpreter\n"
        usage
        putStrLn "\nAvailable options:"
        putStr $ foldr (\opt b -> showOption opt ++ "\n" ++ b) "" opts

die :: IO ()
die = exitWith (ExitFailure 1)

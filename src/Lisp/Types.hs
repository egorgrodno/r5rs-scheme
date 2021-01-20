{-# LANGUAGE DerivingStrategies #-}

module Lisp.Types
  ( IOThrowsException
  , Keyword(..)
  , LispException(..)
  , LispNumber(..)
  , LispVal(..)
  , NArgs(..)
  , PrimProc
  , ProcBody(..)
  , Scope
  , ThrowsException
  , isFalse
  , keywords
  , liftThrow
  , mod''
  ) where

import           Control.Monad.Except           ( ExceptT
                                                , throwError
                                                )
import           Data.Fixed                     ( mod' )
import           Data.IORef                     ( IORef )
import           Data.List.NonEmpty             ( NonEmpty )
import           Prelude
import           Util

type ThrowsException =
  Either LispException

type IOThrowsException =
  ExceptT LispException IO

data LispException =
  InvalidArgs NArgs [LispVal]
  | TypeMismatch String LispVal
  | NotAFunction String
  | BadExpression LispVal
  | CannotApply LispVal
  | GettingUnboundVar String
  | SettingUnboundVar String
  | VarNameReserved String
  | InvalidIdentifier String
  | EmptyBody
  | NonUniqueBinding String

instance Show LispException where
  show = showException

data Keyword =
  KAnd
  | KOr
  | KIf
  | KQuote
  | KDefine
  | KLambda
  | KLet
  | KSet
  | KEq
  | KPlus
  | KMinus
  | KMult
  | KDiv
  | KMod
  | KQuot
  | KRem
  | KLT
  | KGT
  | KLTE
  | KGTE
  | KStrEq
  | KStrLT
  | KStrGT
  | KStrLTE
  | KStrGTE
  deriving stock (Bounded, Enum)

instance Show Keyword where
  show = showKeyword

type Scope =
  [(String, IORef LispVal)]

type PrimProc =
  [LispVal] -> ThrowsException LispVal

data LispVal =
  Atom (Either String Keyword)
  | Number LispNumber
  | Character Char
  | String String
  | Bool Bool
  | List [LispVal]
  | Pair [LispVal] LispVal
  | PrimProc PrimProc
  | Proc ProcBody

data ProcBody =
  ProcBody
    { closure :: IORef Scope
    , params  :: ([String], Maybe String)
    , body    :: NonEmpty LispVal
    }

instance Show LispVal where
  show = showVal

data LispNumber =
  Exact Integer
  | Inexact Double

instance Eq LispNumber where
  (==) = liftBinaryOp (==) (==)

instance Show LispNumber where
  show = showLispNumber

instance Ord LispNumber where
  compare = liftBinaryOp compare compare

instance Num LispNumber where
  (+)         = liftNumBinaryOp (+) (+)
  (-)         = liftNumBinaryOp (-) (-)
  (*)         = liftNumBinaryOp (*) (*)
  abs         = liftNumUnaryOp abs abs
  signum      = liftNumUnaryOp signum signum
  fromInteger = Exact

instance Fractional LispNumber where
  n1 / n2 = case (n1, n2) of
    (Exact   n1', Exact n2'  ) -> if rem n1' n2' == 0
      then Exact $ n1' `div` n2'
      else Inexact $ fromInteger n1' / fromInteger n2'
    (Inexact n1', Inexact n2') -> Inexact $ n1' / n2'
    (Exact   n1', Inexact n2') -> Inexact $ fromInteger n1' / n2'
    (Inexact n1', Exact n2'  ) -> Inexact $ n1' / fromInteger n2'
  fromRational = Inexact . fromRational

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
showException (CannotApply val) =
  "Wrong type to apply: " ++ show val
showException (BadExpression val) =
  "Bad expression: " ++ show val
showException (GettingUnboundVar name) =
  "Getting unbound var " ++ name
showException (SettingUnboundVar name) =
  "Setting unbound var " ++ name
showException (VarNameReserved name) =
  "Use of keyword as variable " ++ name
showException (InvalidIdentifier name) =
  "Invalid identifier " ++ name
showException EmptyBody =
  "Missing body of a procedure"
showException (NonUniqueBinding name) =
  "Non-unique binding " ++ name

liftThrow :: ThrowsException a -> IOThrowsException a
liftThrow (Left  err) = throwError err
liftThrow (Right val) = return val

showKeyword :: Keyword -> String
showKeyword KAnd    = "and"
showKeyword KOr     = "or"
showKeyword KIf     = "if"
showKeyword KQuote  = "quote"
showKeyword KDefine = "define"
showKeyword KSet    = "set!"
showKeyword KEq     = "="
showKeyword KPlus   = "+"
showKeyword KMinus  = "-"
showKeyword KMult   = "*"
showKeyword KDiv    = "/"
showKeyword KMod    = "modulo"
showKeyword KQuot   = "quotient"
showKeyword KRem    = "remainder"
showKeyword KLambda = "lambda"
showKeyword KLet    = "let"
showKeyword KLT     = "<"
showKeyword KGT     = ">"
showKeyword KLTE    = "<="
showKeyword KGTE    = ">="
showKeyword KStrEq  = "string=?"
showKeyword KStrLT  = "string<?"
showKeyword KStrGT  = "string>?"
showKeyword KStrLTE = "string<=?"
showKeyword KStrGTE = "string>=?"

keywords :: [(String, Keyword)]
keywords = [(show k, k) | k <- [minBound .. maxBound]]

showVal :: LispVal -> String
showVal (Atom      e    ) = either id show e
showVal (Number    n    ) = show n
showVal (Character '\n' ) = "#\\newline"
showVal (Character ' '  ) = "#\\space"
showVal (Character c    ) = showCharacter c
showVal (String    a    ) = show a
showVal (Bool      True ) = "#t"
showVal (Bool      False) = "#f"
showVal (List      as   ) = "(" ++ showListVals as ++ ")"
showVal (Pair h t       ) = "(" ++ showListVals h ++ " . " ++ showVal t ++ ")"
showVal (PrimProc _     ) = "#<primitive-procedure>"
showVal (Proc (ProcBody _ (args, arity) _)) =
  let argsStr = unwords args ++ maybe "" (" . " ++) arity
   in "(lambda (" ++ argsStr ++ ") ...)"

showCharacter :: Char -> String
showCharacter =
  ("#\\" ++) . dropStartIf '\\' . dropStartIf '\'' . dropEndIf '\'' . show

showListVals :: [LispVal] -> String
showListVals =
  unwords . map showVal

showLispNumber :: LispNumber -> String
showLispNumber (Exact   a) = show a
showLispNumber (Inexact a) = case break (== '.') (show a) of
  (h, ".0") -> h ++ "."
  (h, t)    -> h ++ t

isFalse :: LispVal -> Bool
isFalse (Bool False) = True
isFalse _            = False

liftNumUnaryOp ::
  (Integer -> Integer)
  -> (Double -> Double)
  -> (LispNumber -> LispNumber)
liftNumUnaryOp f1 f2 n = case n of
  Exact   n' -> Exact $ f1 n'
  Inexact n' -> Inexact $ f2 n'

liftBinaryOp ::
  (Integer -> Integer -> a)
  -> (Double -> Double -> a)
  -> (LispNumber -> LispNumber -> a)
liftBinaryOp f1 f2 n1 n2 = case (n1, n2) of
  (Exact   n1', Exact n2'  ) -> f1 n1' n2'
  (Inexact n1', Inexact n2') -> f2 n1' n2'
  (Exact   n1', Inexact n2') -> f2 (fromInteger n1') n2'
  (Inexact n1', Exact n2'  ) -> f2 n1' (fromInteger n2')

liftNumBinaryOp ::
  (Integer -> Integer -> Integer)
  -> (Double -> Double -> Double)
  -> LispNumber
  -> LispNumber
  -> LispNumber
liftNumBinaryOp f1 f2 =
  liftBinaryOp (\a b -> Exact $ f1 a b) (\a b -> Inexact $ f2 a b)

mod'' :: LispNumber -> LispNumber -> LispNumber
mod'' = liftNumBinaryOp mod mod'

showNArgs :: NArgs -> String
showNArgs (Exactly n) = show n
showNArgs (AtLeast n) = "at least " ++ show n

module Lisp.Prim
  ( LispNumber(..)
  , LispVal(..)
  , mod''
  , showListVals
  ) where

import           Data.Fixed                     ( mod' )

data LispVal =
  Atom String
  | Number LispNumber
  | Character Char
  | String String
  | Bool Bool
  | List [LispVal]
  | Pair [LispVal] LispVal
  deriving Eq

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

showLispNumber :: LispNumber -> String
showLispNumber (Exact   a) = show a
showLispNumber (Inexact a) = case break (== '.') (show a) of
  (h, ".0") -> h ++ "."
  (h, t)    -> h ++ t

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

showVal :: LispVal -> String
showVal (Atom      a    ) = a
showVal (Number    n    ) = show n
showVal (Character '\n' ) = "#\\newline"
showVal (Character ' '  ) = "#\\space"
showVal (Character c    ) = showCharacter c
showVal (String    a    ) = show a
showVal (Bool      True ) = "#t"
showVal (Bool      False) = "#f"
showVal (List      as   ) = "(" ++ showListVals as ++ ")"
showVal (Pair h t       ) = "(" ++ showListVals h ++ " . " ++ showVal t ++ ")"

showCharacter :: Char -> String
showCharacter =
  ("#\\" ++) . dropStartIf '\\' . dropStartIf '\'' . dropEndIf '\'' . show

showListVals :: [LispVal] -> String
showListVals =
  unwords . map showVal

mod'' :: LispNumber -> LispNumber -> LispNumber
mod'' = liftNumBinaryOp mod mod'

-- ---------------------------------------------------------------------------
-- Util

dropStartIf :: Eq a => a -> [a] -> [a]
dropStartIf a str@(h : t) =
  if h == a then t else str
dropStartIf _ [] =
  []

dropEndIf :: Eq a => a -> [a] -> [a]
dropEndIf a [] =
  []
dropEndIf a [b] =
  [b | a /= b]
dropEndIf a (h : t) =
  h : dropEndIf a t

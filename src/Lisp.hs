{-# LANGUAGE LambdaCase #-}

module Lisp
  ( lispExpr
  ) where

import           Control.Applicative            ( (<|>) )
import           Data.Char                      ( digitToInt
                                                , isHexDigit
                                                , isOctDigit
                                                , toLower
                                                )
import           Data.List                      ( find
                                                , foldl'
                                                )
import           Numeric                        ( readHex
                                                , readOct
                                                , readSigned
                                                )
import           Parser


data LispVal =
  Atom String
  | Number Integer
  | String String
  | Bool Bool
  deriving Show

lispExpr :: Parser LispVal
lispExpr =
  lispString
    <|> lispNumber
    <|> lispAtom

lispString :: Parser LispVal
lispString =
  let char =
        character
          >>= (\c1 -> if c1 == '\\'
                then oneOf "\"nrt\\"
                else if c1 == '"'
                  then unexpectedCharParser c1
                  else return c1
              )
   in String <$> between (is '"') (charTok '"') (list char)

symbol :: Parser Char
symbol =
  oneOf "!#$%&|*+-/:<=>?@^_~"

lispAtom :: Parser LispVal
lispAtom =
  (\case
    "#t" -> Bool True
    "#f" -> Bool False
    atom -> Atom atom
  ) <$> (letter <|> symbol) .:. list (letter <|> digit <|> symbol)

lispNumber :: Parser LispVal
lispNumber =
  Number . read <$> list1 digit

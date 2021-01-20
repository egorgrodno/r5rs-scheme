{-# LANGUAGE LambdaCase #-}

module Parser.Char
  ( anyChar
  , char
  , digit
  , eof
  , letter
  , noCaseString
  , noneOf
  , oneOf
  , satisfy
  , satisfyAll
  , satisfyAny
  , space
  , string
  ) where

import           Data.Char                      ( isDigit
                                                , isLetter
                                                , isSpace
                                                , toLower
                                                )
import           Data.Function                  ( on )
import           Parser.Prim
import           Prelude

eof :: Parser ()
eof =
  P (\case
      "" -> Right ("", ())
      i  -> Left $ UnexpectedString i
    )

satisfy :: (Char -> Bool) -> Parser Char
satisfy p =
  P (\case
      ""       -> Left UnexpectedEof
      (c : cs) -> if p c
        then Right (cs, c)
        else Left $ UnexpectedChar c
    )

satisfyAll :: [Char -> Bool] -> Parser Char
satisfyAll ps = satisfy (and  . sequence ps)

satisfyAny :: [Char -> Bool] -> Parser Char
satisfyAny ps = satisfy (or  . sequence ps)

char :: Char -> Parser Char
char = satisfy . (==)

anyChar :: Parser Char
anyChar = satisfy $ const True

letter :: Parser Char
letter = satisfy isLetter

digit :: Parser Char
digit = satisfy isDigit

space :: Parser Char
space = satisfy isSpace

oneOf :: String -> Parser Char
oneOf cs = satisfy (`elem` cs)

noneOf :: String -> Parser Char
noneOf cs = satisfy (`notElem` cs)

string :: String -> Parser String
string = traverse char

noCaseString :: String -> Parser String
noCaseString = traverse (satisfy . on (==) toLower)

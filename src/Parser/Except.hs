module Parser.Except
  ( ParseException(..)
  ) where

data ParseException =
  UnexpectedEof
  | ExpectedEof String
  | UnexpectedChar Char
  | UnexpectedString String
  | UnknownError

instance Show ParseException where
  show = showParseException

showParseException :: ParseException -> String
showParseException UnexpectedEof =
  "Unexpected end of stream"
showParseException (ExpectedEof i) =
  "Expected end of stream, but got >" ++ show i ++ "<"
showParseException (UnexpectedChar c) =
  "Unexpected character: '" ++ [c] ++ "'"
showParseException (UnexpectedString s) =
  "Unexpected string: " ++ show s
showParseException UnknownError =
  "Unknown error"

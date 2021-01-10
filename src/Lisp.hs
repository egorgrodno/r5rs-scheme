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
  | Character Char
  | String String
  | Bool Bool
  deriving Show

lispExpr :: Parser LispVal
lispExpr =
  lispCharacter
    <|> lispString
    <|> lispNumber
    <|> lispAtom

characterNames :: [(String, Char)]
characterNames =
  [ ("null"     , '\NUL')
  , ("nul"      , '\NUL')
  , ("soh"      , '\SOH')
  , ("stx"      , '\STX')
  , ("etx"      , '\ETX')
  , ("eot"      , '\EOT')
  , ("enq"      , '\ENQ')
  , ("ack"      , '\ACK')
  , ("bel"      , '\BEL')
  , ("bs"       , '\BS')
  , ("backspace", '\BS')
  , ("ht"       , '\HT')
  , ("tab"      , '\HT')
  , ("lf"       , '\LF')
  , ("linefeed" , '\LF')
  , ("newline"  , '\LF')
  , ("vt"       , '\VT')
  , ("ff"       , '\FF')
  , ("page"     , '\FF')
  , ("np"       , '\FF')
  , ("cr"       , '\CR')
  , ("return"   , '\CR')
  , ("so"       , '\SO')
  , ("si"       , '\SI')
  , ("dle"      , '\DLE')
  , ("dc1"      , '\DC1')
  , ("dc2"      , '\DC2')
  , ("dc3"      , '\DC3')
  , ("dc4"      , '\DC4')
  , ("nak"      , '\NAK')
  , ("syn"      , '\SYN')
  , ("etb"      , '\ETB')
  , ("can"      , '\CAN')
  , ("em"       , '\EM')
  , ("sub"      , '\SUB')
  , ("esc"      , '\ESC')
  , ("escape"   , '\ESC')
  , ("fs"       , '\FS')
  , ("gs"       , '\GS')
  , ("rs"       , '\RS')
  , ("us"       , '\US')
  , ("sp"       , '\SP')
  , ("space"    , '\SP')
  , ("delete"   , '\DEL')
  , ("del"      , '\DEL')
  , ("rubout"   , '\DEL')
  ]

startsWith :: String -> String -> Bool
startsWith a =
  and . zipWith (==) a

lookupCharacterName :: String -> Maybe (Char, String)
lookupCharacterName name =
  let lowerName = toLower <$> name
      dropMatched (match, a) = (a, drop (length match) name)
   in dropMatched <$> find (\(a,_) -> startsWith a lowerName) characterNames

lispCharacter :: Parser LispVal
lispCharacter =
  let namedCharacter =
        P (\i -> case i of
            "" -> UnexpectedEof
            str -> case lookupCharacterName str of
              Nothing -> UnexpectedString i
              Just (a, rest) -> Result rest a
          )
   in Character <$> (string "#\\" *> (namedCharacter <|> character))

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

lispNumber :: Parser LispVal
lispNumber =
  Number . read <$> list1 digit

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

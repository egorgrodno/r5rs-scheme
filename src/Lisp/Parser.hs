{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE MultiWayIf         #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE DerivingStrategies #-}

module Lisp.Parser
  ( lispExpr
  , lispIdentifier
  ) where

import           Control.Applicative            ( (<|>)
                                                , liftA2
                                                )
import           Data.Bifunctor                 ( second )
import           Data.Char                      ( digitToInt
                                                , isAlpha
                                                , isAlphaNum
                                                , isDigit
                                                , toLower
                                                )
import           Data.List                      ( find
                                                , isPrefixOf
                                                )
import           Numeric                        ( readDec
                                                , readFloat
                                                , readHex
                                                , readOct
                                                )
import           Lisp.Types
import           Parser
import           Prelude                 hiding ( head
                                                , tail
                                                )


data NumberSystem =
  Binary
  | Octal
  | Decimal
  | Hexadecimal
  deriving stock Show

lispExpr :: Parser LispVal
lispExpr =
  let leftParenth = char '(' <* spaces
      rightParenth = spaces *> char ')'
   in lispCharacter
        <|> lispString
        <|> lispNumber
        <|> lispAtom
        <|> lispQuoted
        <|> between leftParenth rightParenth (lispPair <|> lispList)

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
  , ("escape"   , '\ESC')
  , ("esc"      , '\ESC')
  , ("fs"       , '\FS')
  , ("gs"       , '\GS')
  , ("rs"       , '\RS')
  , ("us"       , '\US')
  , ("space"    , '\SP')
  , ("sp"       , '\SP')
  , ("delete"   , '\DEL')
  , ("del"      , '\DEL')
  , ("rubout"   , '\DEL')
  ]

lookupCharacterName :: String -> Maybe (Char, String)
lookupCharacterName name =
  let lowerName = toLower <$> name
      dropMatched (match, a) = (a, drop (length match) name)
   in dropMatched <$> find (\(a,_) -> a `isPrefixOf` lowerName) characterNames

lispCharacter :: Parser LispVal
lispCharacter =
  let namedCharacter =
        P (\i -> case i of
            "" -> Left UnexpectedEof
            str -> case lookupCharacterName str of
              Nothing -> Left $ UnexpectedString i
              Just (a, rest) -> Right (rest, a)
          )
   in Character <$> (string "#\\" *> (namedCharacter <|> anyChar))

lispString :: Parser LispVal
lispString =
  let escapedChar =
        anyChar
          >>= (\c1 -> if c1 == '\\'
                then oneOf "\"nrt\\"
                else if c1 == '"'
                  then unexpectedCharParser c1
                  else return c1
              )
   in String <$> between (char '"') (char '"') (list escapedChar)

numberPrefix :: Parser (NumberSystem, Maybe Bool)
numberPrefix =
  let system =
        Binary <$ noCaseString "#b"
        <|> Octal <$ noCaseString "#o"
        <|> Decimal <$ noCaseString "#d"
        <|> Hexadecimal <$ noCaseString "#x"
      exact = Just True <$ noCaseString "#e"
      inexact = Just False <$ noCaseString "#i"
      pair = liftA2 (,)
      pairR = liftA2 (flip (,))
   in pair system exact
      <|> pair system inexact
      <|> pairR exact system
      <|> pairR inexact system
      <|> (, Nothing) <$> system
      <|> (Decimal,) <$> exact
      <|> (Decimal,) <$> inexact
      <|> returnParser (Decimal, Nothing)

numberParserFromPrefix :: (NumberSystem, Maybe Bool) -> Parser LispNumber
numberParserFromPrefix (system, exact) =
  let f = case exact of
            (Just False) -> Inexact . fromInteger
            _            -> Exact
   in case system of
        Binary      -> f <$> parseSigned parseBinary
        Octal       -> f <$> parseSigned (parserFromReadS readOct)
        Hexadecimal -> f <$> parseSigned (parserFromReadS readHex)
        Decimal     -> case exact of
          (Just True)  -> Exact <$> parseSigned (parserFromReadS readDec)
          (Just False) -> Inexact <$> parseSigned (parserFromReadS readFloat)
          Nothing      -> P (\i -> if isFloat i
                              then second Inexact <$> parse (parseSigned $ parserFromReadS readFloat) i
                              else second Exact <$> parse (parseSigned $ parserFromReadS readDec) i
                            )

lispNumber :: Parser LispVal
lispNumber =
  fmap Number $ numberPrefix >>= numberParserFromPrefix

symbol :: Parser Char
symbol =
  oneOf "!#$%&|*+-/:<=>?@^_~"

lispAtom :: Parser LispVal
lispAtom =
  (\case
    "#t" -> Bool True
    "#f" -> Bool False
    atom -> Atom $ maybe (Left atom) Right (lookup atom keywords)
  ) <$> (letter <|> symbol) .:. list (letter <|> digit <|> symbol)

lispList :: Parser LispVal
lispList =
  List <$> sepBy lispExpr spaces1

lispPair :: Parser LispVal
lispPair = do
  head <- sepBy1 lispExpr spaces1
  tail <- spaces1 *> char '.' *> spaces1 *> lispExpr
  return $ Pair head tail

lispQuoted :: Parser LispVal
lispQuoted =
  (\x -> List [Atom (Right KQuote), x]) <$> (char '\'' *>  lispExpr)

lispIdentifier :: Parser String
lispIdentifier =
  let h       = satisfyAny [isAlpha,    (`elem`          symbols)]
      t       = satisfyAny [isAlphaNum, (`elem` "+-." ++ symbols)]
      symbols = "*/<=>!?:$%_&~^"
   in h .:. list t

-- ---------------------------------------------------------------------------
-- Util

isFloat :: String -> Bool
isFloat "" = False
isFloat (a : as)
  | a == '.'        = True
  | not $ isDigit a = False
  | otherwise       = isFloat as

isBinary :: Char -> Bool
isBinary '0' = True
isBinary '1' = True
isBinary _   = False

digitToInteger :: Char -> Integer
digitToInteger =
  toInteger . digitToInt

parseBinary' :: Integer -> Parser Integer
parseBinary' acc =
  (Just <$> anyChar <|> returnParser Nothing)
    >>= (\case
          Nothing -> return acc
          Just c  -> if
            | isBinary c -> parseBinary' $ acc * 2 + digitToInteger c
            | isDigit c  -> unexpectedCharParser c
            | otherwise  -> returnParserWithInput (c :) acc
        )

{- HLINT ignore parseBinary "Redundant multi-way if" -}
parseBinary :: Parser Integer
parseBinary =
  satisfy isBinary >>= parseBinary' . digitToInteger

parseSigned :: Num a => Parser a -> Parser a
parseSigned (P p) =
  P (\case
      ('-' : i) -> second negate <$> p i
      i         -> p i
    )

parserFromReadS :: ReadS a -> Parser a
parserFromReadS f =
  P (\i -> case f i of
      ((a, t) : _) -> Right (t, a)
      _            -> Left $ UnexpectedString i
    )

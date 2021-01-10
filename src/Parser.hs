{-# LANGUAGE LambdaCase #-}

module Parser
  ( between
  , betweenCharTok
  , betweenSepByComma
  , charTok
  , character
  , constantParser
  , digit
  , eof
  , is
  , letter
  , list
  , list1
  , noneOf
  , oneOf
  , parse
  , satisfy
  , satisfyAll
  , satisfyAny
  , sepBy
  , sepBy1
  , space
  , spaces
  , spaces1
  , string
  , stringTok
  , tok
  , unexpectedCharParser
  , valueParser
  ) where

import           Control.Applicative            ( Alternative
                                                , (<|>)
                                                , empty
                                                , liftA2
                                                )
import           Control.Monad                  ( MonadPlus
                                                , mplus
                                                , mzero
                                                , void
                                                )
import           Data.Char                      ( isDigit
                                                , isLetter
                                                , isSpace
                                                )


type Input =
  String

data ParseResult a =
  UnexpectedEof
  | ExpectedEof Input
  | UnexpectedChar Char
  | UnexpectedString String
  | UnknownError
  | Result Input a
  deriving Eq

instance Show a => Show (ParseResult a) where
  show UnexpectedEof =
    "Unexpected end of stream"
  show (ExpectedEof i) =
    "Expected end of stream, but got >" ++ show i ++ "<"
  show (UnexpectedChar c) =
    "Unexpected character: " ++ show [c]
  show (UnexpectedString s) =
    "Unexpected string: " ++ show s
  show UnknownError =
    "Unknown error"
  show (Result i a) =
    "Result >" ++ show i ++ "< " ++ show a

instance Functor ParseResult where
  fmap _ UnexpectedEof =
    UnexpectedEof
  fmap _ (ExpectedEof a) =
    ExpectedEof a
  fmap _ (UnexpectedChar a) =
    UnexpectedChar a
  fmap _ (UnexpectedString a) =
    UnexpectedString a
  fmap _ UnknownError =
    UnknownError
  fmap f (Result i a) =
    Result i (f a)

isErrorResult :: ParseResult a -> Bool
isErrorResult (Result _ _) = False
isErrorResult _            = True

onResult :: ParseResult a -> (Input -> a -> ParseResult b) -> ParseResult b
onResult UnexpectedEof _ =
  UnexpectedEof
onResult (ExpectedEof a) _ =
  ExpectedEof a
onResult (UnexpectedChar a) _ =
  UnexpectedChar a
onResult (UnexpectedString a) _ =
  UnexpectedString a
onResult UnknownError _ =
  UnknownError
onResult (Result i a) g =
  g i a

newtype Parser a =
  P (Input -> ParseResult a)

parse :: Parser a -> Input -> ParseResult a
parse (P p) =
  p

constantParser :: ParseResult a -> Parser a
constantParser =
  P . const

unexpectedCharParser :: Char -> Parser a
unexpectedCharParser =
  constantParser . UnexpectedChar

valueParser :: a -> Parser a
valueParser a =
  P (`Result` a)

instance Semigroup a => Semigroup (Parser a) where
  (<>) =
    liftA2 (<>)

instance (Monoid a) => Monoid (Parser a) where
  mempty =
    pure mempty

(.:.) :: Parser a -> Parser [a] -> Parser [a]
(.:.) =
  liftA2 (:)

infixr 5 .:.

instance Functor Parser where
  fmap f (P p) =
    P ((f <$>) . p)

instance Applicative Parser where
  pure =
    valueParser
  k <*> p =
    parserBind k (<$> p)
  p1 *> p2 =
    p1 `parserBind` const p2
  p1 <* p2 =
    do { x1 <- p1 ; void p2 ; return x1 }

instance Monad Parser where
  (>>=) =
    parserBind

parserBind :: Parser a -> (a -> Parser b) -> Parser b
parserBind (P p) g =
  P (flip onResult (\i a -> parse (g a) i) . p)

instance MonadPlus Parser where
  mzero =
    parserZero
  mplus =
    parserPlus

parserZero :: Parser a
parserZero =
  constantParser UnknownError

parserPlus :: Parser a -> Parser a -> Parser a
parserPlus (P p1) (P p2) =
  P (\i -> let p1r = p1 i
            in if isErrorResult p1r
                 then p2 i
                 else p1r
    )

instance Alternative Parser where
  empty =
    mzero
  (<|>) =
    mplus

eof :: Parser ()
eof =
  P (\case
      "" -> Result "" ()
      i  -> UnexpectedString i
    )

character :: Parser Char
character =
  P (\case
      (c : cs) -> Result cs c
      []       -> UnexpectedEof
    )

satisfy :: (Char -> Bool) -> Parser Char
satisfy p =
  character
    >>= (\c -> if p c
          then valueParser c
          else unexpectedCharParser c
        )

satisfyAll :: [Char -> Bool] -> Parser Char
satisfyAll ps =
  satisfy (and  . sequence ps)

satisfyAny :: [Char -> Bool] -> Parser Char
satisfyAny ps =
  satisfy (or  . sequence ps)

is :: Char -> Parser Char
is =
  satisfy . (==)

letter :: Parser Char
letter =
  satisfy isLetter

digit :: Parser Char
digit =
  satisfy isDigit

space :: Parser Char
space =
  satisfy isSpace

oneOf :: String -> Parser Char
oneOf cs =
  satisfy (`elem` cs)

noneOf :: String -> Parser Char
noneOf cs =
  satisfy (`notElem` cs)

list :: Parser a -> Parser [a]
list p =
  list1 p <|> valueParser []

list1 :: Parser a -> Parser [a]
list1 p =
  p .:. list p

spaces :: Parser String
spaces =
  list space

spaces1 :: Parser String
spaces1 =
  list1 space

tok :: Parser a -> Parser a
tok p =
  p <* spaces

charTok :: Char -> Parser Char
charTok =
  tok . is

string :: String -> Parser String
string =
  traverse is

stringTok :: String -> Parser String
stringTok =
  tok . string

between :: Parser o -> Parser c  -> Parser a -> Parser a
between l r p =
  l *> p <* r

betweenCharTok :: Char -> Char -> Parser a -> Parser a
betweenCharTok l r =
  between (charTok l) (charTok r)

sepBy1 :: Parser a -> Parser s -> Parser [a]
sepBy1 p s =
  p .:. list (s *> p)

sepBy :: Parser a -> Parser s -> Parser [a]
sepBy p s =
  sepBy1 p s <|> valueParser []

betweenSepByComma :: Char -> Char -> Parser a -> Parser [a]
betweenSepByComma l r p =
  betweenCharTok l r (sepBy (tok p) (charTok ','))

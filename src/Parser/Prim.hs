{-# LANGUAGE TupleSections #-}

module Parser.Prim
  ( Input
  , ParseException(..)
  , ParseResult
  , Parser(..)
  , constantParser
  , parse
  , returnParser
  , returnParserWithInput
  , unexpectedCharParser
  , unexpectedStringParser
  ) where

import           Control.Applicative            ( Alternative(..)
                                                , (<|>)
                                                , liftA2
                                                )
import           Control.Monad                  ( MonadPlus(..)
                                                , void
                                                )
import           Data.Bifunctor                 ( second )
import           Data.Either                    ( isLeft )
import           Parser.Except                  ( ParseException(..) )
import           Prelude

newtype Parser a =
  P (Input -> ParseResult a)

type Input =
  String

type ParseResult a =
  Either ParseException (Input, a)

instance Semigroup a => Semigroup (Parser a) where
  (<>) = liftA2 (<>)

instance (Monoid a) => Monoid (Parser a) where
  mempty = pure mempty

instance Functor Parser where
  fmap f (P p) = P (fmap (second f) . p)

instance Applicative Parser where
  pure = returnParser
  k <*> p = parserBind k (<$> p)
  p1 *> p2 = p1 `parserBind` const p2
  p1 <* p2 = do { x1 <- p1 ; void p2 ; return x1 }

instance Monad Parser where
  (>>=) = parserBind

instance Alternative Parser where
  empty = mzero
  (<|>) = mplus

instance MonadPlus Parser where
  mzero = parserZero
  mplus = parserPlus

parse :: Parser a -> Input -> ParseResult a
parse (P p) = p

onResult :: ParseResult a -> (Input -> a -> ParseResult b) -> ParseResult b
onResult (Left err) _ = Left err
onResult (Right f ) g = uncurry g f

constantParser :: ParseResult a -> Parser a
constantParser = P . const

unexpectedCharParser :: Char -> Parser a
unexpectedCharParser = constantParser . Left . UnexpectedChar

unexpectedStringParser :: String -> Parser a
unexpectedStringParser = constantParser . Left . UnexpectedString

returnParser :: a -> Parser a
returnParser a = P (return . (, a))

returnParserWithInput :: (Input -> Input) -> a -> Parser a
returnParserWithInput f a = P (\i -> return (f i, a))

parserBind :: Parser a -> (a -> Parser b) -> Parser b
parserBind (P p) g = P (flip onResult (\i a -> parse (g a) i) . p)

parserZero :: Parser a
parserZero = constantParser $ Left UnknownError

parserPlus :: Parser a -> Parser a -> Parser a
parserPlus (P p1) (P p2) =
  P (\i -> let p1r = p1 i
            in if isLeft p1r
                 then p2 i
                 else p1r
    )

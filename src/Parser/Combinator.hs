module Parser.Combinator
  ( (.:.)
  , between
  , list
  , list1
  , sepBy
  , sepBy1
  ) where

import           Control.Applicative            ( (<|>)
                                                , liftA2
                                                )
import           Parser.Prim
import           Prelude

(.:.) :: Parser a -> Parser [a] -> Parser [a]
(.:.) = liftA2 (:)

infixr 5 .:.

list :: Parser a -> Parser [a]
list p = list1 p <|> returnParser []

list1 :: Parser a -> Parser [a]
list1 p = p .:. list p

between :: Parser o -> Parser c -> Parser a -> Parser a
between l r p = l *> p <* r

sepBy :: Parser a -> Parser s -> Parser [a]
sepBy p s = sepBy1 p s <|> returnParser []

sepBy1 :: Parser a -> Parser s -> Parser [a]
sepBy1 p s = p .:. list (s *> p)

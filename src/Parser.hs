module Parser
  ( module Parser.Char
  , module Parser.Combinator
  , module Parser.Except
  , module Parser.Prim
  , parse'
  , spaces
  , spaces1
  ) where

import           Data.Bifunctor                 ( first )
import           Except
import           Parser.Char
import           Parser.Combinator
import           Parser.Except
import           Parser.Prim
import           Prelude

parse' :: Parser a -> Input -> IOThrows AppException (Input, a)
parse' p = liftThrow . first ParseException . parse p

spaces :: Parser ()
spaces = () <$ list space

spaces1 :: Parser ()
spaces1 = () <$ list1 space

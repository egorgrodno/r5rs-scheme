{-# LANGUAGE TupleSections #-}

module Util
  ( bind2
  , dropEndIf
  , dropStartIf
  , dup
  , secondM
  ) where

import qualified Data.Set                      as Set
import           Prelude

bind2 :: Monad m => (a1 -> a2 -> m r) -> m a1 -> m a2 -> m r
bind2 g m1 m2 = do
  r1 <- m1
  r2 <- m2
  g r1 r2

secondM :: Monad m => (a -> m b) -> (c, a) -> m (c, b)
secondM g (c, a) = (c,) <$> g a

dropStartIf :: Eq a => a -> [a] -> [a]
dropStartIf a str@(h : t) = if h == a then t else str
dropStartIf _ []          = []

dropEndIf :: Eq a => a -> [a] -> [a]
dropEndIf _ []      = []
dropEndIf a [b    ] = [ b | a /= b ]
dropEndIf a (h : t) = h : dropEndIf a t

dup :: Ord a => [a] -> Maybe a
dup as =
  let dup' []      _ = Nothing
      dup' (h : t) s = if Set.member h s
                        then Just h
                        else dup' t (Set.insert h s)
   in dup' as Set.empty

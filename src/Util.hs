module Util where

import qualified Data.Set                      as Set

bind2 :: Monad m => (a1 -> a2 -> m r) -> m a1 -> m a2 -> m r
bind2 f m1 m2 = do
  r1 <- m1
  r2 <- m2
  f r1 r2

dropStartIf :: Eq a => a -> [a] -> [a]
dropStartIf a str@(h : t) = if h == a then t else str
dropStartIf _ []          = []

dropEndIf :: Eq a => a -> [a] -> [a]
dropEndIf a []      = []
dropEndIf a [b    ] = [ b | a /= b ]
dropEndIf a (h : t) = h : dropEndIf a t

dup :: Ord a => [a] -> Maybe a
dup xs =
  let dup' [] _     = Nothing
      dup' (x:xs) s = if Set.member x s
                        then Just x
                        else dup' xs (Set.insert x s)
   in dup' xs Set.empty

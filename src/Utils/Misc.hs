module Utils.Misc
  where

modulo :: (Num a, Ord a) => a -> a -> a
modulo a b = until (<b) (subtract b) a

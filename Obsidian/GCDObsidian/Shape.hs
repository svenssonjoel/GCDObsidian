{-# LANGUAGE TypeOperators,
             GADTs,
             FlexibleInstances #-}

module Shape where

import Data.Word


data Z
data tail :. head

infixl 3  :.


data Shape a where
  Z :: Shape Z
  (:.) :: Shape sh -> Word32 -> Shape (sh :. Word32)

type DIM0 = Z
type DIM1 = DIM0 :. Word32
type DIM2 = DIM1 :. Word32
type DIM3 = DIM2 :. Word32


dim :: Shape sh -> Int
dim Z = 0
dim (sh :. _) = dim sh + 1

size :: Shape sh -> Word32
size Z = 1
size (sh :. l) = size sh * l 

class Shapely sh where
  mkShape :: Word32 -> Shape sh
  toShape :: Int -> [Word32] -> Shape sh

instance Shapely Z where
  mkShape _ = Z
  toShape _ _ = Z

instance Shapely sh => Shapely (sh :. Word32) where
  mkShape i = mkShape i :. i
  toShape i arr =
    toShape (i+1) arr :. (arr !! (fromIntegral i))

    
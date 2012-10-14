{-# LANGUAGE TypeOperators,
             GADTs,
             FlexibleInstances,
             TypeFamilies #-}

module Obsidian.GCDObsidian.Shape where

import Data.Word

import Obsidian.GCDObsidian.Exp 

data Z
data tail :. head

infixl 3  :.

data Shape sh e where   
  Z :: Shape Z e
  (:.) :: Shape sh e -> e -> Shape (sh :. e) e

type family E x
type instance E Z = Z 
type instance E Word32 = Exp Word32 
type instance E (a :. b) = E a :. E b 


type DIM0 = Z
type DIM1 = DIM0 :. Word32
type DIM2 = DIM1 :. Word32
type DIM3 = DIM2 :. Word32


dim :: Shape sh e -> Int
dim Z = 0
dim (sh :. _) = dim sh + 1

size :: Integral e => Shape sh e -> Word32
size Z = 1
size (sh :. l) = size sh * (fromIntegral l) 


-- Turn into an index into a flat array 
toIndex :: Shape sh Word32 
        -> Shape (E sh) (Exp Word32) 
        -> Exp Word32 
toIndex Z _ = 0
toIndex (sh1 :. sh2) (i1 :. i2) = toIndex sh1 i1 * (fromIntegral sh2) + i2 

fromIndex :: Shape sh Word32 -> Exp Word32 -> Shape (E sh) (Exp Word32) 
fromIndex Z _ = Z 
fromIndex sh@(_:._) i = fromIndexOne sh i 

fromIndexOne :: Shape (sh :. Word32) Word32 
             -> (Exp Word32) 
             -> Shape (E (sh :. Word32)) (Exp Word32) 
fromIndexOne (Z:._) ix = Z :. ix 
fromIndexOne (ds@(_:._) :. d) ix = fromIndexOne ds (ix `quot` d') :. (ix `rem` d')  
    where d' = fromIntegral d 


toBIndex :: Shape gdim Word32
            -> Shape bdim Word32
            -> Shape (E gdim) (Exp Word32)
            -> Exp Word32
toBIndex gdim bdim bix = toBIndex' gdim bix * blockSize 
  where
    blockSize = fromIntegral $ size bdim 

    toBIndex' :: Shape gdim Word32
                 -> Shape (E gdim) (Exp Word32)
                 -> Exp Word32
    toBIndex' Z _ = 0
    toBIndex' (sh1 :. sh2) (i1 :. i2) = ((toBIndex' sh1 i1) + fromIntegral sh2 * i2)   

listShape :: Shapely sh => [Word32] -> Shape sh Word32
listShape = toShape 0 
    
class Shapely sh where
  mkShape :: Word32 -> Shape sh Word32
  toShape :: Int -> [Word32] -> Shape sh Word32

instance Shapely Z where
  mkShape _ = Z
  toShape _ _ = Z

instance Shapely sh => Shapely (sh :. Word32) where
  mkShape i = mkShape i :. i
  toShape i arr =
    toShape (i+1) arr :. (arr !! (fromIntegral i))


class Indexible sh where
  mkIndex :: Shape sh Word32 -> [Exp Word32] -> Shape (E sh) (Exp Word32)

instance Indexible Z where
  mkIndex Z [] = Z

instance Indexible sh => Indexible (sh :. Word32)  where
  mkIndex (sh1 :. sh2) (x:xs) = mkIndex sh1 xs :. x 
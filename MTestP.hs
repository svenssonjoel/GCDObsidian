
module MTestP where 

import Obsidian.GCDObsidian
import Obsidian.GCDObsidian.Printing

import qualified Obsidian.GCDObsidian.CodeGen.CUDA as CUDA

import Prelude hiding (zipWith,splitAt)

import Data.Bits
import Data.Word


----------------------------------------------------------------------------
--  

small1 :: (Array (Data Int),Array (Data Int)) -> Kernel (Array (Data Int))
small1 (arr1,arr2) = pSyncArrayP part
  where
    part = concP arr1' arr2'  
    arr1' = toArrayP arr1
    arr2' = toArrayP arr2 


showSmall1 = printCode$ snd$ runKernel (small1 (namedArray "apa" 32,namedArray "apa" 32))

getSmall1 = putStrLn$ CUDA.genKernel "small1" small1 (namedArray "apa" 32,namedArray "aba" 32)
  


small2 :: (Array (Data Int),Array (Data Int)) -> Kernel (Array (Data Int))
small2 (arr1,arr2) = pSyncArrayP part
  where
    part = concP arr1' arr2'
    arr1' = toArrayP $ zipWith (+) arr1 arr2
    arr2' = toArrayP $ zipWith (-) arr1 arr2
  
    

showSmall2 = printCode$ snd$ runKernel (small2 (namedArray "apa" 32,namedArray "aba" 32))

getSmall2 = putStrLn$ CUDA.genKernel "small2" small2 (namedArray "apa" 32,namedArray "aba" 32)

comp :: Array (Data Int) -> ArrayP (Data Int)
comp arr = part
  where
    (arr1,arr2) = evenOdds arr
    part = zipP arr1' arr2'
    arr1' = toArrayP $ zipWith min arr1 arr2
    arr2' = toArrayP $ zipWith max arr1 arr2

getSmall3 = putStrLn$ CUDA.genKernel "comp" (pure comp ->- pSyncArrayP) (namedArray "apa" 32)

evenOdds :: Array a -> (Array a, Array a)
evenOdds arr = (Array (\ix -> arr ! (2*ix)) (n-n2),
                Array (\ix -> arr ! (2*ix + 1)) n2)
  where
    n = fromIntegral (len arr)
    n2 = div n 2

zipP :: ArrayP a -> ArrayP a -> ArrayP a  
zipP (ArrayP f n1) (ArrayP g n2) =
  ArrayP (\func -> ProgramSeq (f (\i -> func (2*i)))
                              (g (\i -> func (2*i + 1))))
         (n1+n2)



ivDiv :: Int -> Int -> Array a -> (Array a, Array a)
ivDiv i j arr = (Array (\ix -> arr ! newix0 i j ix) (n-n2),
                 Array (\ix -> arr ! newix1 i j ix) n2)
  where 
    n1 = n-n2
    n2 = div n 2
    n = len arr
    newix0 i j ix = ix + (ix .&. complement (fromIntegral (tij - 1)))
    tij = 2^(i+j)
    newix1 i j ix = (newix0 i j ix) `xor` (fromIntegral j')
    j' = (((2^(j+1))-1) :: Word32) `shiftL` i

ivMerge :: Int -> Int -> ArrayP a -> ArrayP a -> ArrayP a
ivMerge i j (ArrayP f n1) (ArrayP g n2) = 
  ArrayP (\func -> ProgramSeq (f (\ix -> func (newix0 i j ix)))
                              (g (\ix -> func (newix1 i j ix))))
          (n1+n2)
  where
    newix0 i j ix = ix + (ix .&. complement (fromIntegral (tij - 1)))
    tij = 2^(i+j)
    newix1 i j ix = (newix0 i j ix) `xor` (fromIntegral j')
    j' = (((2^(j+1))-1) :: Word32) `shiftL` i

iv i j f g arr = part
  where
    (arr1,arr2) = ivDiv i j arr
    part = ivMerge i j arr1' arr2'
    arr1' = toArrayP $ zipWith f arr1 arr2
    arr2' = toArrayP $ zipWith g arr1 arr2



vsort :: Int -> Array (Exp Int) -> Kernel (Array (Exp Int))
vsort n = composeS [ pure (iv (n-i) (i-j) min max)| i <- [1..n], j <- [1..i]]


composeS [] = pure id
composeS (f:fs) = f ->- pSyncArrayP ->- composeS fs

getVsort = putStrLn$ CUDA.genKernel "vsort" (vsort 5) (namedArray "apa" 32)

{--


--}
import Obsidian.GCDObsidian
import Obsidian.GCDObsidian.Kernel

import Obsidian.GCDObsidian.Sync

import qualified Obsidian.GCDObsidian.CodeGen.CUDA   as CUDA
-- import qualified Obsidian.GCDObsidian.CodeGen.OpenCL as OpenCL

import Prelude hiding (zipWith,splitAt)
import Data.Word
import Data.Bits
import Data.List


{-
iv :: Elem a => Int -> Int -> (Exp b -> Exp b-> Exp a) -> (Exp b -> Exp b -> Exp a) -> Array (Exp b) -> Array (Exp a)
iv i j f g arr = Array ixf (len arr)
  where
    ij = i + j
    j' = (((2^(j+1))-1) :: Word32) `shiftL` i
    ixf ix = let l = arr ! ix
                 r = arr ! newix
                 newix = ix `xor` (fromIntegral j')
             in (ifThenElse ((ix .&. (fromIntegral (2^ij))) ==* 0) (f l r) (g l r))
-}

------------------------------------------------------------------------------
-- Generate the indices! 
{-
iv_pattern :: Int -> Int -> Int -> ([Int],[Int])
iv_pattern i j n = ([ix 
                    | ix <- [0..n]
                    , (prim ix) .&. (2^(i+j)) == 0], 
                    [ix 
                    | ix <- [0..n]
                    , (prim ix) .&. (2^(i+j)) /= 0])
  where 
    j' = ((2^(j+1))-1) `shiftL` i
    prim ix = ix `xor` j'
                    
iv_pattern' :: Int -> Int -> Int -> ([(Int,Int)],[(Int,Int)])
iv_pattern' i j n = ([(ix,prim ix) 
                    | ix <- [0..n-1]
                    , ix .&. (2^(i+j)) == 0], 
                    [(ix,prim ix) 
                    | ix <- [0..n-1]
                    , ix .&. (2^(i+j)) /= 0])
  where 
    j' = ((2^(j+1))-1) `shiftL` i
    prim ix = ix `xor` j'

isOkPermutation :: Int -> Int -> Int -> Bool   
isOkPermutation i j n = isTotal$ iv_pattern i j n 

isTotal :: ([Int],[Int]) -> Bool 
isTotal (as,bs) =  length (nub (asbs)) == n 
                   && and [i `elem` asbs | i <- [0..(n-1)]] 
  where 
    n = length as + length bs 
    asbs = as++bs
 -}   
------------------------------------------------------------------------------    
--    
    
bmerge :: Int -> Array (Exp Int) -> Kernel (Array (Exp Int))
bmerge n = composeS [pure (iv (n-i) 0 max min)| i <- [1..n]]

runm =
  putStrLn$ CUDA.genKernel "mm" (bmerge 5) (namedArray "apa" 32)

runm' =
  putStrLn$ CUDA.genKernel "mm" (bmerge 9) (namedArray "apa" 512)


vsort :: Int -> Array (Exp Int) -> Kernel (Array (Exp Int))
vsort n = composeS [ pure (iv (n-i) (i-j) min max)| i <- [1..n], j <- [1..i]]

runs = writeFile "sort.cu" $ CUDA.genKernel "vsort" (vsort 9) (namedArray "apa" 512)
runs' = putStrLn$ CUDA.genKernel "vsort" (vsort 9) (namedArray "apa" 512)

------------------------------------------------------------------------------ 

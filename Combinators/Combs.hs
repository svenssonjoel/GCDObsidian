import Obsidian.GCDObsidian
import Obsidian.GCDObsidian.Kernel
import Obsidian.GCDObsidian.Store
import Obsidian.GCDObsidian.Printing
import Obsidian.GCDObsidian.Evaluate

import qualified Obsidian.GCDObsidian.CodeGen.CUDA   as CUDA
import qualified Obsidian.GCDObsidian.CodeGen.OpenCL as OpenCL
import qualified Obsidian.GCDObsidian.CodeGen.C as C 

import Prelude hiding (zipWith,splitAt)
import Data.Word
  

import Data.Bits



iv :: Choice a => Int -> Int -> (b -> b-> a) -> (b -> b -> a) -> Array b -> Array a
iv i j f g arr = Array ixf (len arr)
  where
    ij = i + j
    j' = (((2^(j+1))-1) :: Int) `shiftL` i
    ixf ix = let l = arr ! ix
                 r = arr ! newix
                 newix = ix `xor` (fromIntegral j')
             in (ifThenElse ((ix .&. (fromIntegral (2^ij))) >* 0) (f l r) (g l r))


bmerge :: Int -> Array (Data Int) -> Kernel (Array (Data Int))
bmerge n = composeS [pure (iv (n-i) 0 min max)| i <- [1..n]]

runm =
  putStrLn$ CUDA.genKernel "mm" (bmerge 5) (namedArray "apa" 32)



-- don't know if a final store (or syncThreads) is needed?
composeS [f] = f
composeS (f:fs) = f ->- store ->- composeS fs

compose [f] = f
compose (f:fs) = f ->- compose fs

composeG [f] s = f 
composeG (f:fs) s = f ->- s ->- compose fs




-- similar to twoK
-- I have trouble deciding if I want the parameter to indicate the size
-- of each block or the number of blocks
-- Trying this old version for now
gpar :: Int -> (Array (Data a) -> Array (Data b)) -> Array (Data a) -> Array (Data b)
-- 2^k is the number of copies of f
gpar k f = (\arr -> 
              let arr'  = Array (\ix -> (f (Array (\j -> (arr ! (perm' ix j))) bLen)) ! (getj ix)) larr   
                  larr = len arr
                  numBlock = 2^k
                  bLen = div larr numBlock
                  oz = larr - bLen
                  perm' ix j = (ix .&. fromIntegral oz) .|. j
                  getj ix = ix .&. fromIntegral (bLen-1)
              in arr')



runp = writeFile "two.cu" $ CUDA.genKernel "vsort" (pure (gilv 5 (rev :: Array (Data Int) -> Array (Data Int)))) (namedArray "apa" 512)



gilv :: Int -> (Array (Data a) -> Array (Data b)) -> Array (Data a) -> Array (Data b)
-- 2^k is the number of copies of f
gilv k f = (\arr -> 
              let arr'  = Array (\ix -> (f (Array (\j -> (arr ! (perm (getl ix) j))) bLen)) ! (getj ix)) larr   
                  larr = len arr
                  numBlock = 2^k 
                  bLen = div larr numBlock
                  perm l j = (j `shiftL` k) .|. l
                  getl ix = ix .&. (fromIntegral (numBlock - 1))
                  getj ix = ix `shiftR` k
              in arr')

integerLogBase b i =
     if i < b then
        0
     else
        -- Try squaring the base first to cut down the number of divisions.
        let l = 2 * integerLogBase (b*b) i
            doDiv :: Int -> Int -> Int
            doDiv i l = if i < b then l else doDiv (i `div` b) (l+1)
        in  doDiv (i `div` (b^l)) l


bmerge1 :: Int -> Array (Data Int) -> Kernel (Array (Data Int))
bmerge1 n = composeS [pure (gilv (n-i) (gpar (i-1) cswapA)) | i <- [1..n]]


bmerge2 :: Int -> Array (Data Int) -> Kernel (Array (Data Int))
bmerge2 n = composeS [pure (gpar (i-1) (gilv (n-i) cswapA)) | i <- [1..n]]

bmerge3 :: Int -> Array (Data Int) -> Kernel (Array (Data Int))
bmerge3 n = composeS [pure (gpar i xblock) | i <- [0..(n-1)]]

run7 =
  putStrLn$ CUDA.genKernel "mm" (bmerge1 5) (namedArray "apa" 32)


run8 =
  putStrLn$ CUDA.genKernel "mm" (bmerge2 5) (namedArray "apa" 32)

run9 =
  putStrLn$ CUDA.genKernel "mm" (bmerge3 5) (namedArray "apa" 32)


-- should be the same as gilv n
ilvs n = gilv (n-1) (gilv 1 cswapA)

run10 = 
  putStrLn$ CUDA.genKernel "mm" (pure (ilvs 5)) (namedArray "apa" 32)



xblock :: Array (Data Int) -> Array (Data Int)
xblock arr = conc (a1',a2')
  where
    (a1,a2) = splitAt middle arr
    middle = len arr `div` 2 
    a1' = zipWith min a1 a2
    a2' = zipWith max a1 a2

------------------------------------------------------------------------------
-- Added by js 
xblock' :: Array (Data Int) -> Array (Data (Int,Int))
xblock' arr = zipp (a1',a2')
  where
    (a1,a2) = splitAt middle arr
    middle = len arr `div` 2 
    a1' = zipWith min a1 a2
    a2' = zipWith max a1 a2


bmerge3' :: Int -> Array (Data Int) -> Kernel (Array (Data Int))
bmerge3' n = compose [pure (twoK i xblock') ->- storeCatZ | i <- [0..(n-1)]]

run9' =
  putStrLn$ CUDA.genKernel "mm" (bmerge3' 5) (namedArray "apa" 32)


------------------------------------------------------------------------------


cswapA :: Array (Data Int) -> Array (Data Int)
cswapA arr = Array (\ix -> let l = arr ! ix
                               r = arr ! (xor ix 1)
                           in (ifThenElse ((ix .&. 1) >* 0) (max l r) (min l r))) (len arr)


-- perhaps this is bad style?
-- maybe everything should be able to deal with all arrays whose length
-- is a power of two
cswap2 :: Array (Data Int) -> Array (Data Int)
cswap2 arr = Array (\ix -> let l = arr ! ix
                               r = arr ! (xor ix 1)
                           in (ifThenElse (ix >* 0) (max l r) (min l r))) 2
  



-- untested, may just have broken it
gvee :: Int -> (Array (Data a) -> Array (Data b)) -> Array (Data a) -> Array (Data b)
-- 2^k is the length of each block to which f is applied
gvee k f = (\arr -> 
              let arr'  = Array (\ix -> (f (Array (\j -> (arr ! (perm' ix j))) bLen)) ! (getj ix)) larr   
                  larr = len arr
                  numBlock = 2^k 
                  bLen = div larr numBlock
                  numBlock' = fromIntegral (numBlock - 1)
                  perm' ix j = (j `shiftL` k) .|. l'
                    where
                      l' = (xor ix (- (j .&. 1) `xor` numBlock') ) .&. numBlock'
                  getj ix = ix `shiftR` k
              in arr')



run11 =
  putStrLn$ CUDA.genKernel "mm" (pure (gvee 4  cswapA)) (namedArray "apa" 32)




vsort :: Int -> Array (Data Int) -> Kernel (Array (Data Int))
vsort n = composeS [ pure (iv (n-i) (i-j) max min)| i <- [1..n], j <- [1..i]]

runs = writeFile "sort.cu" $ CUDA.genKernel "vsort" (vsort 9) (namedArray "apa" 512)

runc = writeFile "sort.c" $ C.genKernel "vsort" (vsort 9) (namedArray "apa" 512)


vsort1 :: Int -> Array (Data Int) -> Kernel (Array (Data Int))
vsort1 n = composeS [ pure (gilv  (n-i)(gvee  (i-j) cswapA))| i <- [1..n], j <- [1..i]]

runs1 = writeFile "vsort1.cu" $ CUDA.genKernel "vsort" (vsort1 9) (namedArray "apa" 512)

import Obsidian.GCDObsidian

import qualified Obsidian.GCDObsidian.CodeGen.CUDA as CUDA


import Prelude hiding (zipWith,splitAt)

import Data.Word
import Data.Bits
import Data.List hiding (zipWith,splitAt)


-- for Library?  (or wherever composeS is defined ?)


compose :: (Scalar a) => [Array (Exp a) -> Array (Exp a)] -> Array (Exp a) -> Kernel (Array (Exp a))
compose = composeS . map pure

composeP :: (Scalar a) => [Array (Exp a) -> ArrayP (Exp a)] -> Array (Exp a) -> Kernel (Array (Exp a))
composeP = composeS . map pure






-- first, bitonic merge

bmerge :: Int -> [Array IntE -> Array IntE]
bmerge n = [stage (n-i) | i <- [1..n]]
  where
    stage i = ilv1 i min max


-- I showed this call in the paper
runm k =
  putStrLn$ CUDA.genKernel "bitonicMerge" (compose (bmerge k)) (namedArray "inp" (2^k))

-- Here is a general version
rungen k s f = 
  putStrLn$ CUDA.genKernel s (compose (f k)) (namedArray "inp" (2^k))



-- Next, replace the first ilv by a vee to get a merger that sorts two concatenated sorted lists (tmerge)
-- ilv1 and vee1 are in Library.hs

tmerge :: Int -> [Array IntE -> Array IntE]
tmerge n = vstage (n-1) : [ istage (n-i) | i <- [2..n]]
  where
    vstage i = vee1 i min max
    istage i = ilv1 i min max

runt k = rungen k "tMerge" tmerge
  



-- Because tmerge sorts two half-length sorted lists, it is easy to compose a tree of them to make a sorter (tsort1)

tsort1 :: Int -> [Array IntE -> Array IntE]
tsort1 n = concat [tmerge i | i <- [1..n]]

writegen k s f = 
  writeFile (s ++ ".cu") $ CUDA.genKernel s (compose (f k)) (namedArray "inp" (2^k))

runs1 k = writegen k "tsort1" tsort1





-- Next step is to play with push arrays. Need to reimplement ilv1 and vee1.
-- See ilv2 and vee2 in Library.hs


tmerge2 :: Int -> [Array IntE -> ArrayP IntE]
tmerge2 n = vee2 (n-1) min max : [(ilv2 (n-i) min max)| i <- [2..n]]


tsort2 :: Int -> [Array IntE -> ArrayP IntE]
tsort2 n = concat [tmerge2 i | i <- [1..n]]


-- Unfortunately, now I have to use composeP.
writes2 k = writeFile "tsort2.cu" $ CUDA.genKernel "tsort2" (composeP (tsort2 k)) (namedArray "inp" (2^k))



-- Next balanced bitonic merger

bpmerge2 :: Int -> [Array IntE -> ArrayP IntE]
bpmerge2 n = [vee2 (n-i) min max | i <- [1..n]]

runbp =
  putStrLn$ CUDA.genKernel "bpMerge" (composeP (bpmerge2 5)) (namedArray "inp" 32)



-- Now to vsort

-- ilvVee1 (without Push arrays) and ilvVee2 (with) are in Library.hs


vsort1 :: Int -> Array IntE -> Kernel (Array IntE)
vsort1 n = compose [ istage(n-i) (i-j) | i <- [1..n], j <- [1..i]]
  where
    istage i j = ilvVee1 i j min max

writevsort1 k = writeFile "vsort1.cu" $ CUDA.genKernel "vsort1" (vsort1 k) (namedArray "apa" (2^k))



-- This is the fastest kernel
vsort :: Int -> Array IntE -> Kernel (Array IntE)
vsort n = composeS . map pure $ [ istage (n-i) (i-j) | i <- [1..n], j <- [1..i]]
  where
    istage i j = ilvVee2 i j min max


writevsort k = writeFile "vsort.cu" $ CUDA.genKernel "vsort" (vsort k) (namedArray "inp" (2^k))


halve' arr = splitAt ((len arr) `div` 2) arr
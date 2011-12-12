import Obsidian.GCDObsidian

import qualified Obsidian.GCDObsidian.CodeGen.CUDA as CUDA
import qualified Obsidian.GCDObsidian.CodeGen.C as C
import qualified Obsidian.GCDObsidian.CodeGen.OpenCL as CL

import Obsidian.GCDObsidian.Program

import Data.Word
import Data.Bits


import Prelude hiding (zipWith,sum )




-- for Library?  


compose :: (Scalar a) => [Array Pull (Exp a) -> Array Pull (Exp a)] -> Array Pull (Exp a) -> Kernel (Array Pull (Exp a))
compose = composeS . map pure

composeP :: (Scalar a) => [Array Pull (Exp a) -> Array Push (Exp a)] -> Array Pull (Exp a) -> Kernel (Array Pull (Exp a))
composeP = composeS . map pure






-- first, bitonic merge

bmerge :: Int -> [Array Pull IntE -> Array Pull IntE]
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

tmerge :: Int -> [Array Pull IntE -> Array Pull IntE]
tmerge n = vstage (n-1) : [ istage (n-i) | i <- [2..n]]
  where
    vstage i = vee1 i min max
    istage i = ilv1 i min max

runt k = rungen k "tMerge" tmerge
  



-- Because tmerge sorts two half-length sorted lists, it is easy to compose a tree of them to make a sorter (tsort1)

tsort1 :: Int -> [Array Pull IntE -> Array Pull IntE]
tsort1 n = concat [tmerge i | i <- [1..n]]

writegen k s f = 
  writeFile (s ++ ".cu") $ CUDA.genKernel s (compose (f k)) (namedArray "inp" (2^k))

runs1 k = writegen k "tsort1" tsort1





-- Next step is to play with push arrays. Need to reimplement ilv1 and vee1.
-- See ilv2 and vee2 in Library.hs


tmerge2 :: Int -> [Array Pull IntE -> Array Push IntE]
tmerge2 n = vee2 (n-1) min max : [(ilv2 (n-i) min max)| i <- [2..n]]


tsort2 :: Int -> [Array Pull IntE -> Array Push IntE]
tsort2 n = concat [tmerge2 i | i <- [1..n]]


-- Unfortunately, now I have to use composeP.
writes2 k = writeFile "tsort2.cu" $ CUDA.genKernel "tsort2" (composeP (tsort2 k)) (namedArray "inp" (2^k))


bmerge2 :: Int -> [Array Pull IntE -> Array Push IntE]
bmerge2 n = [stage (n-i) | i <- [1..n]]
  where
    stage i = ilv2 i min max

writes2b k = writeFile "bmerge2.cu" $ CUDA.genKernel "bmerge2" (composeP (bmerge2 k)) (namedArray "inp" (2^k))


bmerge4 :: Int -> [Array Pull IntE -> Array Push IntE]
bmerge4 n | odd n = [ilv4 (n-1) min max] ++ [ilv42 (n-2*i) min max | i <- [1.. (div n 2)]] 
bmerge4 n |even n = [ilv42 (n-2*i+1) min max | i <- [1.. (div n 2)]] 
  where
    stage i = ilv42 i min max

writes4b k = writeFile "bmerge4N.cu" $ CUDA.genKernel_ "bmerge4" (composeP (bmerge4 k)) (namedArray "inp" (2^k))
print4b k = putStrLn $ CUDA.genKernel "bmerge4" (composeP (bmerge4 k)) (namedArray "inp" (2^k))
print4b_ k = putStrLn $ CUDA.genKernel_ "bmerge4" (composeP (bmerge4 k)) (namedArray "inp" (2^k))



-- Next balanced periodic merger

bpmerge2 :: Int -> [Array Pull IntE -> Array Push IntE]
bpmerge2 n = [vee2 (n-i) min max | i <- [1..n]]

runbp =
  putStrLn$ CUDA.genKernel "bpMerge" (composeP (bpmerge2 5)) (namedArray "inp" 32)



-- Now to vsort

-- ilvVee1 (without Push arrays) and ilvVee2 (with) are in Library.hs


vsort1 :: Int -> Array Pull IntE -> Kernel (Array Pull IntE)
vsort1 n = compose [ istage(n-i) (i-j) | i <- [1..n], j <- [1..i]]
  where
    istage i j = ilvVee1 i j min max

writevsort1 k = writeFile "vsort1.cu" $ CUDA.genKernel "vsort1" (vsort1 k) (namedArray "apa" (2^k))
printvsort1 k = putStrLn $ CUDA.genKernel "vsort1" (vsort1 k) (namedArray "apa" (2^k))
printvsort1_ k = putStrLn $ CUDA.genKernel_ "vsort1" (vsort1 k) (namedArray "apa" (2^k))



-- This is the fastest kernel
vsort :: Int -> Array Pull IntE -> Kernel (Array Pull IntE)
vsort n = composeS . map pure $ [ istage (n-i) (i-j) | i <- [1..n], j <- [1..i]]
  where
    istage i j = ilvVee2 i j min max


writevsort k = writeFile "vsort.cu" $ CUDA.genKernel "vsort" (vsort k) (namedArray "inp" (2^k))
printvsort k = putStrLn $ CUDA.genKernel "vsort" (vsort k) (namedArray "inp" (2^k))
printvsort_ k = putStrLn $ CUDA.genKernel_ "vsort" (vsort k) (namedArray "inp" (2^k))

-- halve' arr = Prelude.splitAt ((len arr) `div` 2) arr

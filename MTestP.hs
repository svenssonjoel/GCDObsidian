
module MTestP where 

import Obsidian.GCDObsidian
import Obsidian.GCDObsidian.Printing
import Obsidian.GCDObsidian.Program

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
    arr1' = push arr1
    arr2' = push arr2 


showSmall1 = printCode$ snd$ runKernel (small1 (namedArray "apa" 32,namedArray "bepa" 32))

getSmall1 = putStrLn$ CUDA.genKernel "small1" small1 (namedArray "apa" 32,namedArray "bepa" 32)
  


small2 :: (Array (Data Int),Array (Data Int)) -> Kernel (Array (Data Int))
small2 (arr1,arr2) = pSyncArrayP part
  where
    part = concP arr1' arr2'
    arr1' = push $ zipWith (+) arr1 arr2
    arr2' = push $ zipWith (-) arr1 arr2
  
    

showSmall2 = printCode$ snd$ runKernel (small2 (namedArray "apa" 32,namedArray "aba" 32))

getSmall2 = putStrLn$ CUDA.genKernel "small2" small2 (namedArray "apa" 32,namedArray "aba" 32)

comp :: Array (Data Int) -> ArrayP (Data Int)
comp arr = part
  where
    (arr1,arr2) = evenOdds arr
    part = zipP arr1' arr2'
    arr1' = push $ zipWith min arr1 arr2
    arr2' = push $ zipWith max arr1 arr2

getSmall3 = putStrLn$ CUDA.genKernel "comp" (pure comp ->- pSyncArrayP) (namedArray "apa" 32)


vsort :: Int -> Array (Exp Int) -> Kernel (Array (Exp Int))
vsort n = composeS [ pure (iv (n-i) (i-j) min max)| i <- [1..n], j <- [1..i]]

getVsort = putStrLn$ CUDA.genKernel "vsort" (vsort 5) (namedArray "apa" 32)





----------------------------------------------------------------------------
-- 
small :: (Array (Data Int),Array (Data Int)) -> Kernel (Array (Data Int))
small (arr1,arr2) = ( sync ->- pure (fmap fst)) p2
  where
    part  = zipp (arr1,arr2)  
    p2 = push part
    


showSmall = printCode$ snd$ runKernel (small (namedArray "apa" 32,namedArray "bepa" 32))

getSmall = putStrLn$ CUDA.genKernel "small" small (namedArray "apa" 32,namedArray "bepa" 32)

{-# LANGUAGE FlexibleInstances #-} 
module Examples where 

--import Obsidian.GCDObsidian

--import qualified Obsidian.GCDObsidian.CodeGen.CUDA as CUDA
--import qualified Obsidian.GCDObsidian.CodeGen.C as C
--import qualified Obsidian.GCDObsidian.CodeGen.OpenCL as CL

import Obsidian.GCDObsidian.Program
import Obsidian.GCDObsidian.Array
import Obsidian.GCDObsidian.Exp
--import qualified Obsidian.GCDObsidian.Helpers as Help

import Data.Word
import Data.Bits


import Prelude hiding (zipWith,sum )

instance Functor (Array Pull) where 
  fmap f arr = Array (len arr) (Pull (\ix -> f (arr ! ix)))  

---------------------------------------------------------------------------
-- MapFusion example
mapFusion :: Array Pull IntE -> Program (Array Pull IntE)
mapFusion = return . fmap (+1) . fmap (*2) 

input1 :: Array Pull IntE 
input1 = namedArray "apa" 32

-- getMapFusion   = putStrLn$ CUDA.genKernel "mapFusion" mapFusion input1
-- getMapFusion_  = putStrLn$ CL.genKernel_ "mapFusion" mapFusion input1


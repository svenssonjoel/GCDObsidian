module Examples where 

import Obsidian.GCDObsidian

import qualified Obsidian.GCDObsidian.CodeGen.CUDA as CUDA





mapFusion :: Array IntE -> Kernel (Array IntE) 
mapFusion = pure (fmap (+1) . fmap (*2)) 

inputMapFusion :: Array IntE 
inputMapFusion = namedArray "apa" 32

getMapFusion = putStrLn$ CUDA.genKernel "mapFusion" mapFusion inputMapFusion
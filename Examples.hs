{-# LANGUAGE FlexibleInstances,
             ScopedTypeVariables#-} 
module Examples where 

--import Obsidian.GCDObsidian

--import qualified Obsidian.GCDObsidian.CodeGen.CUDA as CUDA
--import qualified Obsidian.GCDObsidian.CodeGen.C as C
--import qualified Obsidian.GCDObsidian.CodeGen.OpenCL as CL

import Obsidian.GCDObsidian.Program
import Obsidian.GCDObsidian.Array
import Obsidian.GCDObsidian.Exp
import Obsidian.GCDObsidian.Types
--import qualified Obsidian.GCDObsidian.Helpers as Help

import Data.Word
import Data.Bits


import Prelude hiding (zipWith,sum )

instance Functor (Array Pull) where 
  fmap f arr = Array (len arr) (Pull (\ix -> f (arr ! ix)))  

---------------------------------------------------------------------------
-- MapFusion example
mapFusion :: Array Pull IntE -> Program (Array Pull IntE)
mapFusion arr =
  do
    a1 <- sync $ (fmap (+1) . fmap (*2)) arr
    sync $ (fmap (+1) . fmap (*2)) a1

input1 :: Array Pull IntE 
input1 = namedArray "apa" 32


sync = force . push 

push :: Array Pull a -> Array Push a
push (Array n (Pull ixf)) =
  Array n $ Push $
  \k -> ForAll n (\i -> (k (i,ixf i)))


force :: Array Push (Exp Int) -> Program (Array Pull (Exp Int))
force (Array n (Push p)) =
  do 
    name <- Allocate n Int  -- force needs to be in a Class of Forceables..
    p (targetArr name)
    return $ Array n $ Pull (\i -> index name i)
    where
      targetArr name (i,e) = Assign name i e 

-- getMapFusion   = putStrLn$ CUDA.genKernel "mapFusion" mapFusion input1
-- getMapFusion_  = putStrLn$ CL.genKernel_ "mapFusion" mapFusion input1

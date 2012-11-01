{-# LANGUAGE FlexibleInstances #-} 
--              ScopedTypeVariables#-} 
module Examples where 

--import Obsidian.GCDObsidian

--import qualified Obsidian.GCDObsidian.CodeGen.CUDA as CUDA
--import qualified Obsidian.GCDObsidian.CodeGen.C as C
--import qualified Obsidian.GCDObsidian.CodeGen.OpenCL as CL

import Obsidian.GCDObsidian.Program
import Obsidian.GCDObsidian.Array
import Obsidian.GCDObsidian.Exp
import Obsidian.GCDObsidian.Types
import Obsidian.GCDObsidian.Blocks 
--import qualified Obsidian.GCDObsidian.Helpers as Help

import Data.Word
import Data.Bits


import Prelude hiding (zipWith,sum )

instance Functor (Array Pull) where 
  fmap f arr = Array (len arr) (Pull (\ix -> f (arr ! ix)))  


---------------------------------------------------------------------------
-- MapFusion example
---------------------------------------------------------------------------


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

-- requires much type class magic
mapBlocks' :: Scalar a => (Array Pull (Exp a) -> Program (Array Push (Exp b)))
             -> Blocks (Array Pull (Exp a))
             -> Blocks (Program (Array Push (Exp b)))
mapBlocks' f (Blocks nb s bxf) =
  Blocks nb newSize (\bix -> (f (bxf bix)))
   -- how to obtain newSize ?
   -- newSize depends on the Kernel.
   -- But Kernels always have the same number of outputs.
   -- so running one instance of the kernel on a dummy
   -- array will give the answer.
   where (Array newSize _,_)  = (runPrg 0 (f dummy))
         dummy = Array s (Pull (\ix -> index "dummy" ix))

zipBlocksWith' :: (Scalar a, Scalar b) 
                  => (Array Pull (Exp a) -> Array Pull (Exp b) -> Program (Array Push (Exp c)))
                  -> Blocks (Array Pull (Exp a))
                  -> Blocks (Array Pull (Exp b))
                  -> Blocks (Program (Array Push (Exp c)))
zipBlocksWith' f (Blocks nb1 s1 bxf1)
                 (Blocks nb2 s2 bxf2) =
  Blocks (min nb1 nb2) newSize (\bix -> f (bxf1 bix) (bxf2 bix))
    where
      (Array newSize _,_)  = (runPrg 0 (f dummy1 dummy2))
      dummy1 = Array s1 (Pull (\ix -> index "dummy1" ix))
      dummy2 = Array s2 (Pull (\ix -> index "dummy2" ix))

-- forceBlocks :: Blocks (Program a) -> Program (Blocks a)
-- cannot be this general.. 

-- Trying a very limited form, will need type classes... 
forceBlocks :: Blocks (Program (Array Push (Exp Int)))
               -> Program (Blocks (Array Pull (Exp Int)))
forceBlocks (Blocks n s bxf) =  
  do
    global <- Output Int -- type class magic
    
    ForAllBlocks n
      (\bid ->
        do
          (Array _ (Push pfun)) <- bxf bid 
          pfun (assignTo global (bid, s)))
    return $ Blocks n s $ 
             \bix -> Array s (Pull (\ix -> index global ((bix * (fromIntegral s)) + ix)))

          where 
            assignTo name (bid,s) (i,e) = Assign name ((bid*(fromIntegral s))+i) e 


pushApp (Array n (Push p)) a = p a 

-- getMapFusion   = putStrLn$ CUDA.genKernel "mapFusion" mapFusion input1
-- getMapFusion_  = putStrLn$ CL.genKernel_ "mapFusion" mapFusion input1

---------------------------------------------------------------------------
-- Global array permutation
---------------------------------------------------------------------------
rev :: Array Pull IntE -> Array Pull IntE
rev (Array n (Pull ixf)) = Array n (Pull (\ix -> ixf (ix - 1 - (fromIntegral n))))

reverseG :: Blocks (Array Pull IntE) -> Blocks (Array Pull IntE)
reverseG (Blocks nb s arrf) =
  Blocks nb s (\bix -> rev (arrf (nb - 1 - bix)))


-- Permutations on the output arrays are more complicated
-- good wrappings are needed!
reverseGO :: Blocks (Program (Array Push IntE))
             -> Blocks (Program (Array Push IntE))
reverseGO (Blocks nb s prgf) =
  Blocks nb s
  (\bix -> do
      a@(Array n (Push p)) <- prgf bix
      let k' k (ix,e) = k ((fromIntegral n) - 1 - ix,e)
      return (Array n (Push (\k -> p (k' k)))))  
      -- k :: (Exp Word32,IntE) -> Program
---------------------------------------------------------------------------
-- Global Array examples 
---------------------------------------------------------------------------

mapSomething :: Array Pull IntE -> Program (Array Push IntE)
mapSomething arr =return $  push ((fmap (+1) . fmap (*2)) arr)



inputG :: Blocks (Array Pull IntE) 
inputG = namedGlobal "apa" (variable "N") 32



testG1 :: Blocks (Array Pull IntE) -> Program (Blocks (Array Pull IntE))
testG1 arr = forceBlocks ( mapBlocks' mapSomething (reverseG arr) )



---------------------------------------------------------------------------
-- Print Programs for test
---------------------------------------------------------------------------
prg0 = putStrLn$ (\(_,x,_) -> x) $ printPrg 0 (mapFusion input1)
prg1 = putStrLn$ (\(_,x,_) -> x) $ printPrg 0 (testG1 inputG)
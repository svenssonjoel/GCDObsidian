{-# LANGUAGE FlexibleInstances,
             MultiParamTypeClasses #-} 

module Examples where 

import Obsidian.GCDObsidian.Array
import Obsidian.GCDObsidian.Shape

import Obsidian.GCDObsidian.Kernel
import Obsidian.GCDObsidian.Exp
import Obsidian.GCDObsidian.Program

import Obsidian.GCDObsidian.Library
import Obsidian.GCDObsidian.Types
import Obsidian.GCDObsidian.Globs


-- import qualified Obsidian.GCDObsidian.CodeGen.CUDA as CUDA

-- import qualified Obsidian.GCDObsidian.CodeGen.C as C
-- import qualified Obsidian.GCDObsidian.CodeGen.OpenCL as CL

-- import Obsidian.GCDObsidian.Program
-- import qualified Obsidian.GCDObsidian.Helpers as Help

-- import Data.Word
-- import Data.Bits

import Control.Monad.Writer
import Control.Monad.State

import Prelude hiding (zipWith,sum, reverse)
import Data.Word

{-
 TODOS:
   TOO MANY TO MENTION!
-} 

---------------------------------------------------------------------------
--
---------------------------------------------------------------------------
mapFusion :: ArrayPull DIM1 IntE -> ArrayPull DIM1 IntE 
mapFusion = fmap (+1) . fmap (*2)  

codeMapFusion = force $ mapFusion input1 

input1 :: ArrayPull DIM1 IntE 
input1 = namedArray (listShape [256]) "apa"

codeMapFusion2 =
  do
    p <- force $ mapFusion input1
    q <- force $ mapFusion p 
    return$ pushWithBid q (Z :. BlockIdx)  
   

---------------------------------------------------------------------------
-- force a computation (aka sync) 
---------------------------------------------------------------------------
-- TODO: force :: Pushable something => something dim a -> P (Pull dim a)  
force :: Scalar a => Pull DIM1 (Exp a)  -> P (Pull DIM1 (Exp a))
force  pully@(Pull bsh ixf) =
  P $ \k ->
    do
      nom <- newName "arr"
      ((unP . pushFun . push) pully) (assignImm nom) *>>>
        (k (Pull bsh (\i -> index nom (toIndex bsh i))))
        where
          assignImm imm (dummy,ix,e) =
            return (Assign imm (t oIndex bsh ix) e) 

---------------------------------------------------------------------------
-- push (make into a Push array) 
---------------------------------------------------------------------------
push :: Pull DIM1 (Exp a) -> Push DIM1 (Exp a)
push (Pull n f) = Push Z n (push' n f) 

push' :: Shape sh Word32
         -> (Shape (E sh) (Exp Word32) -> Exp a)
         -> P (Shape Z (Exp Word32), Shape (E sh) (Exp Word32), Exp a)
push' bsh ixf =
  P $ \k -> do func <- runFunc k
               return (ForAll (size bsh)
                              (\i -> func (mkShape 0,
                                           fromIndex bsh i,
                                           ixf (fromIndex bsh i))))
---------------------------------------------------------------------------
-- push (make into a Push array) 
---------------------------------------------------------------------------

--blockMap :: (Pull DIM1 a -> P (Pull DIM1 b))
--            -> PullG GDIM1 DIM1 a -> PushG GDIM1 DIM1 b
--blockMap f iarr@(PullG gsh bsh gixf) =
--  PushG gsh bsh $
--  do
    
--    P (\k ->
                      

pushWithBid :: Pull DIM1 (Exp a)
               -> Shape (E DIM1) (Exp Word32)  -> PushG (E DIM1) DIM1 (Exp a) 
pushWithBid (Pull bsh ixf) gsh =
  Push gsh bsh $ P $ \k ->
  do func <- runFunc k
     return (ForAllGlobal (size bsh)
             (\bid tix-> func (fromIndexDyn gsh bid,
                               fromIndex bsh tix,
                               ixf (fromIndex bsh tix))))
                                               
                          
 
{- 
getMapFusion  = putStrLn$ CUDA.genKernel "mapFusion" mapFusion input1
getMapFusion_ = putStrLn$ CUDA.genKernel_ "mapFusion" mapFusion input1
                 
-- GLOBAL ARRAYS 

mapFusionG :: PullG GDIM1 DIM1 IntE -> Kernel (PullG GDIM1 DIM1 IntE)
mapFusionG = pure (fmap (+2) . fmap (*4))

input2 :: PullG GDIM1 DIM1 IntE 
input2 = namedGlobal (listShape [variable "d1"]) (listShape [256]) "apa" 

getMapFusionG  = putStrLn$ CUDA.genKernel "mapFusion" mapFusionG input2

getMapFusionG_  = putStrLn$ CUDA.genKernel_ "mapFusion" mapFusionG input2


local_f :: Pull DIM1 IntE -> Pull DIM1 IntE
local_f = fmap (+2)

global_f :: PullG GDIM1 DIM1 IntE -> Kernel (PushG GDIM1 DIM1 IntE)
global_f garr = pure (pBlocks (pullGGridDim garr) . local_f . blocks) garr


-- blocks :: PullG (DIM2 -> ... ) DIM2
-- problem domain vs distribution of work onto (grid decomposition)

-- TODO: TESTA!
--blocks ::  (Pull DIM1 a -> Pull DIM1 b) -> PullG GDIM1 DIM1  a -> PullG GDIM1 DIM1 b


--  PullG (Shape sh0 Word32, Shape sh1 (Exp Word32)) a  
-- I dont like having to put a hardcoded "BlockIdx" in this code.
-- But maybe thats fine ?? ... 
blocks (PullG gsh bsh gixf) = Pull bsh $ \tix -> gixf (fromIndexDyn gsh BlockIdx) tix
-} 
{- 
blockMap :: (Pull DIM1 a -> Kernel (Pull DIM1 b))
            ->  PullG GDIM1 DIM1 a -> Kernel (PushG GDIM1 DIM1 b)
blockMap f input@(PullG gsh bsh gixf) =
  do
    imm <- f (getBlock input)
    return $ res imm
  where 
    res imm = mkPushG gsh bsh $ \k ->
      ForAllGlobal (fromIntegral (size bsh))
                   (\bix tix ->
                     let bix' = fromIndexDyn gsh bix
                         tix' = fromIndex bsh tix
                     in  k (bix',tix',imm tix'))
-}                          
--pushBlockTo :: Push DIM1 b -> Kernel (Exp Word32 -> PushG GDIM1 DIM1 b)
--pushBlockTo (Push bsh pixf) =
--  return $ \bix -> PushG 
      

      --pull <- f (Pull bsh (\ix -> 
      --PullG gsh bsh $ \bix tix -> 


---------------------------------------------------------------------------
-- 
---------------------------------------------------------------------------
{- 
-- create a global push array given a shape-of-blocks
-- and a local array (a block) 
pBlocks :: Shape gsh (Exp Word32) -> Pull DIM1 a -> PushG gsh DIM1 a
pBlocks gsh (Pull bsh ixf) =
  mkPushG gsh bsh
  $ \k -> ForAllGlobal -- (size gsh)
                       (size bsh)
                       $ \bix tix -> (k (fromIndexDyn gsh bix,
                                         fromIndex bsh tix,
                                         ixf (fromIndex bsh tix)))

getGlobal_f  = putStrLn$ CUDA.genKernel "global_f" global_f input2 

global_f2 :: PullG GDIM1 DIM1 IntE -> Kernel (PushG GDIM1 DIM1 IntE)
global_f2 garr = (pure blocks ->- myLocal ->- pure (pBlocks (pullGGridDim garr))) garr

myLocal :: Pull DIM1 IntE -> Kernel (Pull DIM1 IntE)
myLocal = pure (fmap (+1)) ->-
          pSyncArray ->-
          pure (fmap (*2)) ->-
          pSyncArray ->-
          pure (fmap (+3)) ->-
          pSyncArray ->-
          pure (fmap (*4)) 


getGlobal_f2 = putStrLn$ CUDA.genKernel "global_f2" global_f2 input2 

---------------------------------------------------------------------------
-- Sync Experiments
---------------------------------------------------------------------------
pSyncArray  :: Scalar a => Pull sh (Exp a) -> Kernel (Pull sh (Exp a))
pSyncArray arr@(Pull sh ixf) = 
  do 
    name <- newArray
    
    let p = ((unP . pushFun) parr) (newTargetArray sh name)
         
    tell$ 
        (Allocate name (es * (size sh)) t ()) 
        `ProgramSeq`
        p 
        `ProgramSeq`
        (Synchronize True)
            
    return$ Pull sh (\tix -> index name (toIndex sh tix))
      
  where 
    es = fromIntegral$ sizeOf (arr ! (fromIndex sh 0)) 
    t  = Pointer$ Local$ typeOf (arr ! (fromIndex sh 0))

    parr = toPush arr 
        

newTargetArray :: Scalar a
                  => (Shape sh Word32)
                  -> Name
                  ->  (Shape (E sh) (Exp Word32),Exp a)
                  -> Program ()
newTargetArray sh n (i,a) = Assign n (toIndex sh i) a 

-} 
{- 
---------------------------------------------------------------------------
--Reifyable functions
---------------------------------------------------------------------------
class ReifyableFun a b where
  reifyFun :: (a -> b) -> a -> State (Integer,Integer)
                                     ([(String,Type,Word32)], --inputs 
                                      [(String,Type,Word32)], --outputs 
                                      Program ())

---------------------------------------------------------------------------
--
---------------------------------------------------------------------------

newInputID = do
  (i,o) <- get
  put (i+1,o)
  return $ "input" ++ show i

newOutputID = do
  (i,o) <- get
  put (i,o+1)
  return $ "output" ++ show o

---------------------------------------------------------------------------
--
---------------------------------------------------------------------------


-- Now generalise this. 
instance ReifyableFun (PullG gsh bsh (Exp Int))
                      (Kernel (PushG gsh1 bsh1 (Exp Int))) where
  reifyFun f (PullG gsh bsh e) =
    do
      inID <- newInputID
      outID <- newOutputID
      let inArr = PullG gsh bsh
                        (\bix tix -> indexG  inID
                                             (fromIntegral (size bsh))
                                             (toIndex gsh bix)
                                             (toIndex bsh tix))

          -- needs to know things about the result
          -- in order to generate the code that writes outputs. 
          rp@(PushG rgsh rbsh rf) = evalKernel (f inArr)
         
          prog  = getKernelProgram (f inArr)

          -- writeout is a program that writes to an output array. 
          writeout = (unP rf) (newGlobalTarget outID rgsh rbsh)
                        
      -- Write outputs also
      return ([],[],prog *>* writeout)

-- Duplicated code (also in InOut.hs) 
newGlobalTarget nom gsh bsh (bix,tix,a) =
  Assign nom ((toIndex gsh bix * blockSize) + toIndex bsh tix) a
  where
    blockSize = fromIntegral (size bsh)
-} 
---------------------------------------------------------------------------
--
---------------------------------------------------------------------------


-- test = evalState (reifyFun global_f2 input2) (0,0)
      
        

{- 

reverse :: Array Pull IntE -> Array Push IntE 
reverse arr = mkPushArray n $ 
                \k ->
                  ForAll n
                  (\i -> k (m - 1 - i,arr ! i))
  where
    n = len arr
    m = fromIntegral n
    
getReverse   = putStrLn$ CUDA.genKernel "reverse" (pure reverse) input1
getReverse_  = putStrLn$ CUDA.genKernel_ "reverse" (pure reverse) input1
-} 
{- 
---------------------------------------------------------------------------
-- MapFusion example
mapFusion :: Array Pull IntE -> Kernel (Array Pull IntE) 
mapFusion = pure (fmap (+1) . fmap (*2)) 

input1 :: Array Pull IntE 
input1 = namedArray "apa" 32

getMapFusion   = putStrLn$ CUDA.genKernel "mapFusion" mapFusion input1
getMapFusion_  = putStrLn$ CUDA.genKernel_ "mapFusion" mapFusion input1
getMapFusionC  = putStrLn$ C.genKernel "mapFusion" mapFusion input1
getMapFusionCL = putStrLn$ CL.genKernel "mapFusion" mapFusion input1


---------------------------------------------------------------------------
-- mapUnfused example
mapUnFused :: Array Pull IntE -> Kernel (Array Pull IntE) 
mapUnFused = pure (fmap (*2)) ->- sync ->- pure (fmap (+1))

getMapUnFused = putStrLn$ CUDA.genKernel "mapUnFused" mapUnFused input1
getMapUnFused_ = putStrLn$ CUDA.genKernel_ "mapUnFused" mapUnFused input1


---------------------------------------------------------------------------
-- reduction of array of length a power of two
reduce :: Syncable (Array Pull) a => (a -> a -> a) -> Array Pull a -> Kernel (Array Pull a)
reduce op arr | len arr == 1 = return arr
              | otherwise    = 
                (pure ((uncurry (zipWith op)) . halve)
                 ->- sync
                 ->- reduce op) arr


reduceS :: (a -> a -> a) -> Array Pull a -> Kernel (Array Pull a) 
reduceS op arr | len arr == 1 = return arr
               | otherwise    = 
                 (pure ((uncurry (zipWith op)) . halve)
                  ->- reduceS op) arr


input8 :: Array Pull IntE 
input8 = namedArray "input" 8

input16 :: Array Pull IntE 
input16 = namedArray "input" 16


getReduceAdd = putStrLn$ CUDA.genKernel "reduceAdd" (reduce (+)) input8
getReduceAddLarge = putStrLn$ CUDA.genKernel "reduceAdd" (reduce (+)) input256
getReduceAddLargeC = putStrLn$ C.genKernel "reduceAdd" (reduce (+)) input256
getReduceAddLargeCL = putStrLn$ CL.genKernel "reduceAdd" (reduce (+)) input256

getReduceAddC = putStrLn$ C.genKernel "reduceAdd" (reduce (+)) input8                
getReduceSAdd = putStrLn$ CUDA.genKernel "reduceSAdd" (reduceS (+)) input8


catArrays :: (Array Pull (Exp Int), Array Pull (Exp Int)) 
           -> Kernel (Array Pull (Exp Int))
catArrays  = pure conc

getCatArrays = putStrLn$ CUDA.genKernel "catArrays" (catArrays) (input16,input16)


zippUnpair :: (Array Pull IntE, Array Pull IntE) -> Kernel (Array Pull IntE) 
zippUnpair = pure (unpair . zipp)

input32 :: Array Pull IntE 
input32 = namedArray "apa" 32

input64 :: Array Pull IntE 
input64 = namedArray "apa" 64

input128 :: Array Pull IntE
input128 = namedArray "apa" 128


input256 :: Array Pull IntE
input256 = namedArray "apa" 256


getZippUnpair = putStrLn$ CUDA.genKernel "zippUnpair" zippUnpair (input32,input32)


zippUnpairP :: (Array Pull IntE, Array Pull IntE) -> Kernel (Array Push IntE) 
zippUnpairP = pure (unpairP . zipp)

getZippUnpairP = putStrLn$ CUDA.genKernel "zippUnpairP" zippUnpairP (input32,input32)


catArrayPs :: (Array Pull (Exp Int), Array Pull (Exp Int)) 
           -> Kernel (Array Push (Exp Int))
catArrayPs = pure concP -- (arr1,arr2) = return$ concP (arr1, arr2)


getCatArrayPs = putStrLn$ CUDA.genKernel "catArrayPs" (catArrayPs) (input16,input16)





sum :: Array Pull IntE -> Kernel (Array Pull IntE) 
sum arr | len arr == 1 = return arr
        | otherwise    = (pure (fmap (uncurry (+)) . pair) 
                          ->- sync 
                          ->- sum) arr
                         
getSum = putStrLn$ CUDA.genKernel "sum" sum input8                   
getSumIM = snd $ runKernel (sum input8)



---------------------------------------------------------------------------- 
--  GLOBAL ARRAY TESTS 

test1 :: GlobalArray Pull (Exp Int) -> Kernel (GlobalArray Push (Exp Int)) 
test1 = pure (block 256) ->- pure (fst . halve) ->- 
        pure (unblock . push) 

getTest1 = putStrLn$ CUDA.genKernelGlob "test1" test1 (GlobalArray undefined (variable "n"):: GlobalArray Pull (Exp Int)) 


testParam1 :: (GlobalArray Pull (Exp Int), Exp Int) -> Kernel (GlobalArray Push (Exp Int)) 
testParam1 (garr, param) = res
  where 
    res = test1$ fmap (+param) garr 

getTestParam1_ = putStrLn$ CUDA.genKernelGlob_ "testParam1" testParam1 (GlobalArray undefined (variable "n") :: GlobalArray Pull (Exp Int),variable "v") 
getTestParam1 = putStrLn$ CUDA.genKernelGlob "testParam1" testParam1 (GlobalArray undefined (variable "n") :: GlobalArray Pull (Exp Int),variable "v") 


globRev (GlobalArray (Pull ixf) n) = GlobalArray (Pull (\ix -> ixf (n - 1 - ix))) n

testGlobRev = pure globRev ->- 
              pure (block 256) ->- 
              pure rev ->- -- also reverse each block 
              pure (unblock . push) 

getTestGlobRev = putStrLn$ CUDA.genKernelGlob "testGlobRev" testGlobRev (GlobalArray undefined (variable "n") :: GlobalArray Pull (Exp Int)) 
getTestGlobRev_ = putStrLn$ CUDA.genKernelGlob_ "testGlobRev" testGlobRev (GlobalArray undefined (variable "n") :: GlobalArray Pull (Exp Int)) 


vSwap :: (GlobalArray Pull (Exp Int), Exp Word32) -> 
         Kernel (GlobalArray Push (Exp Int)) 
vSwap (arr,stride) = return p5
    
  where 
    t1 ix = ix + (ix .&. (complement (stride - 1)))
    t2 ix = (t1 ix) `xor` ((stride `shiftL` 1)-1)
    arr1  = mkGlobalPullArray (\ix -> arr ! t1 ix) (globLen arr `div` 2)
    arr2  = mkGlobalPullArray (\ix -> arr ! t2 ix) (globLen arr `div` 2)
    arr1' = zipWithG min arr1 arr2
    arr2' = zipWithG max arr1 arr2
    p1    = pushGlobal 512 arr1'
    p2    = pushGlobal 512 arr2'
    p3    = ixMap t1 p1 
    p4    = ixMap t2 p2 
    p5    = GlobalArray (Push (\k -> p3 !* k *>* p4 !* k)) (globLen arr)
    
                                     
vSwap' :: (GlobalArray Pull (Exp Int), Exp Word32) -> 
         Kernel (GlobalArray Push (Exp Int)) 
vSwap' (arr,stride) = 
  do 
    p1 <- pushGlobal' 512 arr1'
    p2 <- pushGlobal' 512 arr2'
    let p3    = ixMap t1 p1 
    let p4    = ixMap t2 p2 
    let p5    = mkGlobalPushArray (\k -> p3 !* k *>* p4 !* k) (globLen arr)
    return p5 
    
  where 
    t1 ix = ix + (ix .&. (complement (stride - 1)))
    t2 ix = (t1 ix) `xor` ((stride `shiftL` 1)-1)
    arr1  = mkGlobalPullArray (\ix -> arr ! t1 ix) (globLen arr `div` 2)
    arr2  = mkGlobalPullArray (\ix -> arr ! t2 ix) (globLen arr `div` 2)
    arr1' = zipWithG min arr1 arr2
    arr2' = zipWithG max arr1 arr2
    -- p1    = pushGlobal 512 arr1'
    -- p2    = pushGlobal 512 arr2'
    -- p3    = ixMap t1 p1 
    -- p4    = ixMap t2 p2 
    -- p5    = GlobalArray (Push (\k -> p3 !* k *>* p4 !* k)) (globLen arr)


zipWithG op a1 a2 =  
  mkGlobalPullArray (\ix -> (a1 ! ix) `op` (a2 ! ix))
                   (min (globLen a1) (globLen a2))

-- a global array is "pushed" by dividing 
-- it up into short pieces that are pushed blockwise. 
pushGlobal blocksize = 
   unblock . push . block blocksize   
  
pushGlobal' blocksize = 
  pure  (block blocksize) ->- sync  ->- pure (unblock . push) 
     
getvSwap = putStrLn$ CUDA.genKernelGlob "vSwap" vSwap (GlobalArray undefined (variable "n") :: GlobalArray Pull (Exp Int),variable "stride")     
getvSwap_ = putStrLn$ CUDA.genKernelGlob_ "vSwap" vSwap (GlobalArray undefined (variable "n") :: GlobalArray Pull (Exp Int),variable "stride")     

getvSwap' = putStrLn$ CUDA.genKernelGlob "vSwap" vSwap' (GlobalArray undefined (variable "n") :: GlobalArray Pull (Exp Int),variable "stride")     
getvSwap'_ = putStrLn$ CUDA.genKernelGlob_ "vSwap" vSwap' (GlobalArray undefined (variable "n") :: GlobalArray Pull (Exp Int),variable "stride")     


----------------------------------------------------------------------------
--
reduceAddBlocks :: GlobalArray Pull (Exp Int) -> Kernel (GlobalArray Push (Exp Int)) 
reduceAddBlocks  = withBlockSize 64 (reduce (+)) 


withBlockSize n p = pure (block n) ->- p ->- pure (unblock . push) 

getR = putStrLn$ CUDA.genKernelGlob "reduce" reduceAddBlocks (GlobalArray undefined (variable "n") :: GlobalArray Pull (Exp Int))     
getR_ = putStrLn$ CUDA.genKernelGlob_ "reduce" reduceAddBlocks (GlobalArray undefined (variable "n") :: GlobalArray Pull (Exp Int))     



----------------------------------------------------------------------------
-- 
apa :: Array Pull (Exp Int) -> Kernel (Array Push (Exp Int))
apa = sync  ->- pure push 


getApa = putStrLn$ CUDA.genKernel "apa" apa (namedArray "hej" 128 :: Array Pull (Exp Int))


----------------------------------------------------------------------------
-- Preloading examples (Breaks because of poor state of Sync.hs) 



-- puts two elements per thread in shared memory
preload2Test :: Array Pull (Exp Int) -> Kernel (Array Pull (Exp Int))
preload2Test = Help.preload2 

getpreload2Test = putStrLn$ CUDA.genKernel "preload2" preload2Test (namedArray "hej" 128 :: Array Pull (Exp Int))

----------------------------------------------------------------------------
--
preload3Test :: Array Pull (Exp Int) -> Kernel (Array Pull (Exp Int))
preload3Test = Help.preload3 

getpreload3Test = putStrLn$ CUDA.genKernel "preload3" preload3Test (namedArray "hej" (3*100) :: Array Pull (Exp Int))


----------------------------------------------------------------------------
-- I'm not sure why this outputs any code right now. 

preload4Test :: Array Pull (Exp Int) -> Kernel (Array Pull (Exp Int))
preload4Test = Help.preload4 

getpreload4Test = putStrLn$ CUDA.genKernel "preload4" preload4Test (namedArray "hej" (4*100) :: Array Pull (Exp Int))

----------------------------------------------------------------------------
-- 
preload2'Test :: Array Pull (Exp Int) -> Kernel (Array Pull (Exp Int))
preload2'Test = Help.preload2' 

getpreload2'Test = putStrLn$ CUDA.genKernel "preload2" preload2'Test (namedArray "hej" 128 :: Array Pull (Exp Int))

----------------------------------------------------------------------------
--
preload3'Test :: Array Pull (Exp Int) -> Kernel (Array Pull (Exp Int))
preload3'Test = Help.preload3' 

getpreload3'Test = putStrLn$ CUDA.genKernel "preload3" preload3'Test (namedArray "hej" (3*100) :: Array Pull (Exp Int))


----------------------------------------------------------------------------
-- 

preload4'Test :: Array Pull (Exp Int) -> Kernel (Array Pull (Exp Int))
preload4'Test = Help.preload4' 

getpreload4'Test = putStrLn$ CUDA.genKernel "preload4" preload4'Test (namedArray "hej" (4*100) :: Array Pull (Exp Int))



----------------------------------------------------------------------------
-- GlobalArrays and preloading 

globalPreloadTest :: GlobalArray Pull (Exp Int) -> Kernel (GlobalArray Push (Exp Int)) 
globalPreloadTest = 
  -- 128 elements per block. 
  withBlockSize 128 
    ( 
      Help.preload4' ->- 
      reduceSeqSteps 2 (+) ->- sync ->- 
      reduce (+) ->- 
      Help.push1 
    ) 

reduceSeqSteps :: Int -> (a -> a -> a) -> Array Pull a -> Kernel (Array Pull a)
reduceSeqSteps 0 op = pure id
reduceSeqSteps n op = pure (uncurry (zipWith op) . halve) ->- reduceSeqSteps (n-1) op

getGlobalPreload = putStrLn$ CUDA.genKernelGlob "globalPreload" globalPreloadTest (GlobalArray undefined (variable "n") :: GlobalArray Pull (Exp Int))     
getGlobalPreload_ = putStrLn$ CUDA.genKernelGlob_ "globalPreload" globalPreloadTest (GlobalArray undefined (variable "n") :: GlobalArray Pull (Exp Int))     

----------------------------------------------------------------------------
-- 
globalPreloadSimple :: GlobalArray Pull (Exp Int) -> Kernel (GlobalArray Push (Exp Int)) 
globalPreloadSimple = 
  -- 128 elements per block. 
  withBlockSize 128 
    ( 
      Help.preload4' ->-   --load 4 elements per thread from GlobArray
      Help.push4           --push 4 elements per threads
    ) 

getGlobalPreloadSimple = putStrLn$ CUDA.genKernelGlob "globalPreloadS" globalPreloadSimple (GlobalArray undefined (variable "n") :: GlobalArray Pull (Exp Int))     
getGlobalPreloadSimple_ = putStrLn$ CUDA.genKernelGlob_ "globalPreloadS" globalPreloadSimple (GlobalArray undefined (variable "n") :: GlobalArray Pull (Exp Int))     

-}
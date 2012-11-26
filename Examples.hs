{-# LANGUAGE FlexibleInstances,
             FlexibleContexts, 
             ScopedTypeVariables,
             RankNTypes  #-} 
module Examples where 

--import Obsidian.GCDObsidian

import qualified Obsidian.GCDObsidian.CodeGen.CUDA as CUDA
import Obsidian.GCDObsidian.CodeGen.CUDA.WithCUDA
import Obsidian.GCDObsidian.CodeGen.CUDA.Compile
import qualified Foreign.CUDA.Driver as CUDA
--import qualified Obsidian.GCDObsidian.CodeGen.C as C
--import qualified Obsidian.GCDObsidian.CodeGen.OpenCL as CL

import qualified Obsidian.GCDObsidian.CodeGen.Program as CGP
import           Obsidian.GCDObsidian.CodeGen.InOut

import Obsidian.GCDObsidian.Program
import Obsidian.GCDObsidian.Exp
import Obsidian.GCDObsidian.Types
-- import Obsidian.GCDObsidian.Blocks
import Obsidian.GCDObsidian.Array
import Obsidian.GCDObsidian.Library
import Obsidian.GCDObsidian.Force


import Data.Word
import Data.Int
import Data.Bits

import qualified Data.Vector.Storable as V

import Control.Monad.State

import Prelude hiding (zipWith,sum,replicate)
import qualified Prelude as P 

---------------------------------------------------------------------------
-- MapFusion example
---------------------------------------------------------------------------

mapFusion :: Array Pull EInt -> Program (Array Pull EInt)
mapFusion arr =
  do
    a1 <- sync $ (fmap (+1) . fmap (*2)) arr
    sync $ (fmap (+1) . fmap (*2)) a1

input1 :: Array Pull EInt 
input1 = namedArray "apa" 32

-- Uses genKernel, that implicitly maps over all blocks. 
-- getMapFusion   = putStrLn$ CUDA.genKernel "mapFusion" mapFusion input1
-- getMapFusion_  = putStrLn$ CL.genKernel_ "mapFusion" mapFusion input1

---------------------------------------------------------------------------
-- Sync, Force. What to use? what to scrap ? 
---------------------------------------------------------------------------
-- very limiting type.. 
sync :: Scalar a => Array Pull (Exp a) -> Program (Array Pull (Exp a))
sync = force

             
---------------------------------------------------------------------------
-- Global array permutation
---------------------------------------------------------------------------

--reverseG :: Blocks (Array Pull a) -> Blocks (Array Pull a)
--reverseG (Blocks nb arrf) =
--  Blocks nb (\bix -> rev (arrf (nb - 1 - bix)))


-- Permutations on the output arrays are more complicated
-- good wrappings are needed!
--reverseGO :: Blocks (Array Push a)
--             -> Blocks (Array Push a)
--reverseGO (Blocks nb prgf) =
--  Blocks nb $ 
--   \bix ->
--    let a@(Array n (Push p)) =  prgf bix
--    in  Array n $
--        Push  (\k ->
--               let k' k (ix,e) = k ((fromIntegral n) - 1 - ix,e)
--               in  p (k' k))
               -- k :: (Exp Word32,EInt) -> Program

---------------------------------------------------------------------------
-- Global Array examples 
---------------------------------------------------------------------------

mapSomething :: Array Pull EInt -> Array Push EInt
mapSomething arr = push ((fmap (+1) . fmap (*2)) arr)



--inputG :: Blocks (Array Pull EInt) 
--inputG = namedGlobal "apa" (variable "N") 256



--testG1 :: Blocks (Array Pull EInt) -> Program (Blocks (Array Pull EInt))
--testG1 arr = force ( fmap mapSomething (reverseG arr) )

--getTestG1 = putStrLn$ CUDA.genKernel "testG1" testG1 inputG

--testG2 :: Blocks (Array Pull EInt)
--          -> Blocks (Array Pull EInt)
--          -> Program (Blocks (Array Pull EInt))
--testG2 _ arr = force ( fmap mapSomething (reverseG arr) )


---------------------------------------------------------------------------
-- Print Programs for test
---------------------------------------------------------------------------
prg0 = putStrLn$ printPrg$ mapFusion input1
--prg1 = putStrLn$ printPrg$ testG1 inputG


---------------------------------------------------------------------------
-- Translate and pring as CGP.Programs 
---------------------------------------------------------------------------
prg0' = putStrLn$ CGP.printPrg$ CGP.runPrg (mapFusion input1)
--prg1' = putStrLn$ CGP.printPrg$ CGP.runPrg (testG1 inputG)

---------------------------------------------------------------------------
-- A small test for the function "reifyer" 
---------------------------------------------------------------------------
--reify0 = fst $ toProgram 0 testG2 (inputG :-> inputG)






---------------------------------------------------------------------------
-- Counting sort experiments
---------------------------------------------------------------------------


---------------------------------------------------------------------------
-- Histogram
---------------------------------------------------------------------------
{- 
histogram :: Exp Word32
             -> Blocks (Array Pull (Exp Word32))
             -> Blocks (Array Push (Exp Word32))
histogram maxLen (Blocks nb blkf)  =
  Blocks nb blkf' 
  where                
    blkf' bid = Array blkSize (Push (collect bid)) 
    blkSize = len (blkf 0) -- all blocks are same size  
    collect bid k = ForAll blkSize $ \i ->
      k ((blkf bid) ! i,1)
-} 
{- 
histogram :: Blocks (Array Pull (Exp Word32))
             -> GlobArray PushBT (Exp Word32)
histogram (Blocks nb blkf) =
  GlobArray nb 256 $ PushP (\wf bix ->
              let arr = blkf bix
                  blkSize = len arr
              in ForAll blkSize $ \i ->
                  let ix' = arr ! i
                      blk = ix' `div` fromIntegral blkSize
                      ix  = ix' `mod` fromIntegral blkSize  
                  in  wf ix 1 blk)
-}

{- 
histogram :: Blocks (Array Pull (Exp Word32))
             -> GlobArray PushBT (Exp Word32)
histogram (Blocks nb blkf) =
  GlobArray nb blkSize $ PushP (\wf bix tix ->
              let arr = blkf bix
                  ix' = arr ! tix
                  blk = ix' `div` fromIntegral blkSize
                  ix  = ix' `mod` fromIntegral blkSize  
              in  wf 1 blk ix)
    where
      blkSize = len (blkf 0)

-}
{-
 

-}
{-
forceBT :: forall a. Scalar a => GlobArray PushBT (Exp a) -> Program (Blocks (Array Pull (Exp a)))
forceBT (GlobArray nb bs (PushP pbt)) =
  do
      global <- Output $ Pointer (typeOf (undefined :: (Exp a)))
      
      ForAllBlocks nb 
        (\bid ->
          ForAll bs $ \ix -> 
            (pbt (assignTo global bs)) bid ix)
        
      return $ Blocks nb  $ 
        \bix -> Array bs (Pull (\ix -> index global ((bix * (fromIntegral bs)) + ix)))
    where 
      assignTo name s e b i = Assign name ((b*(fromIntegral s))+i) e
      
hist
  :: Exp Word32
     -> Blocks (Array Pull (Exp Word32))
     -> Program (Blocks (Array Pull (Exp Word32)))
hist max inp = forceBT (histogram {-max-} inp)

inputWord32 :: Blocks (Array Pull (Exp Word32)) 
inputWord32 = namedGlobal "apa" (variable "N") 256


getHist = putStrLn$ CUDA.genKernel "hist" (hist 256)  inputWord32

-} 
---------------------------------------------------------------------------
-- Scan  (TODO: Rewrite as a exclusive scan (0 as first elem in result) 
---------------------------------------------------------------------------
sklanskyLocal
  :: (Num (Exp a), Scalar a) =>
     Int
     -> (Exp a -> Exp a -> Exp a)
     -> Array Pull (Exp a)
     -> Program (Array Pull (Exp a))
sklanskyLocal 0 op arr = return (shiftRight 1 0 arr)
sklanskyLocal n op arr =
  do 
    let arr1 = twoK (n-1) (fan op) arr
    arr2 <- sync arr1
    sklanskyLocal (n-1) op arr2
                     

fan op arr =  a1 `conc`  fmap (op c) a2 
    where 
      (a1,a2) = halve arr
      c = a1 ! (fromIntegral (len a1 - 1))


sklanskyAllBlocks :: Int
                     -> Distrib (Array Pull (Exp Int32))
                     -> Distrib (Program (Array Pull (Exp Int32)))
sklanskyAllBlocks logbsize arr =
  mapD (sklanskyLocal logbsize (+)) arr
   


--getScan n = CUDA.genKernel "scan" (sklanskyAllBlocks n) 
--                    (namedGlobal "apa" (variable "N") (2^n)
--                     :: Blocks (Array Pull (Exp Int32)))


--getScan_ n = CUDA.genKernel_ "scan" (sklanskyAllBlocks n) 
--                    (namedGlobal "apa" (variable "N") (2^n)
--                     :: Blocks (Array Pull (Exp Int32)))

-- TODO: Rewrite Scan with BlockMap functionality.
--       Also add the output of blockmaxs, and tweak code generation to
--       allow such kernels. 
---------------------------------------------------------------------------
-- Reconstruct
---------------------------------------------------------------------------

-- type GlobalArray p a = Blocks (Array p a) 
{- 
reconstruct :: Blocks (Array Pull (Exp Word32))
               -> Blocks (Array Pull (Exp Word32))
               -> GlobArray PushBT (Exp Word32)
reconstruct inp pos =
  GlobArray nb bs $
    PushP $ \wf bix tix ->
        let gix = (inp !| bix) ! tix
            bix' = gix `div` (fromIntegral bs)
            tix' = gix `mod` (fromIntegral bs)

            pgix = (pos !| bix') ! tix'
            pbix = pgix `div` (fromIntegral bs)
            ptix = pgix `mod` (fromIntegral bs) 
            
        in wf gix pbix ptix
  where
    bs = len (inp !| 0)
    (Blocks nb _) = inp

recs :: Blocks (Array Pull (Exp Word32))
        -> Blocks (Array Pull (Exp Word32))
        -> Program (Blocks (Array Pull (Exp Word32)))
recs inp pos = forceBT (reconstruct inp pos) 
-}
{-
  Blocks nb (\bix -> -- nb seems totally unimportant
                     -- (does appear in code). rethink this.
    Array bsize
    $ Push (\k -> do
        ForAll bsize
          $ \ ix ->
          let bix' = ((inp !| bix) ! ix) `div` (fromIntegral bsize)
              ix'  = ((inp !| bix) ! ix) `mod` (fromIntegral bsize)
          in 
              k ((pos !| bix') ! ix',
                 (inp !| bix) ! ix)))
                         
          --(\ix -> k ((pos !| bix) ! ix , 
          --           (inp !| bix) ! ix))))
  where
    bsize = len (inp !| 0)
-} 
{- 
reconstruct :: Exp Word32
               -> GlobalArray Pull (Exp Word32)
               -> GlobalArray Pull (Exp Word32)
               -> Program (GlobalArray Pull (Exp Word32))
reconstruct nb inp pos = force $ 
  Blocks nb (\bix -> -- nb seems totally unimportant
                     -- (does appear in code). rethink this.
    Array bsize
    $ Push (\k -> do
        ForAll bsize
          $ \ ix ->
          let bix' = ((inp !| bix) ! ix) `div` (fromIntegral bsize)
              ix'  = ((inp !| bix) ! ix) `mod` (fromIntegral bsize)
          in 
              k ((pos !| bix') ! ix',
                 (inp !| bix) ! ix)))
                         
          --(\ix -> k ((pos !| bix) ! ix , 
          --           (inp !| bix) ! ix))))
  where
    bsize = len (inp !| 0)
-} 
-- Check that reconstruct does what it is suppoed to
-- TODO: Needs type convertion functionality if this is
--       to be implemented for anything else than Word32.
--         (indices are Word32)
{-
getReconstruct n =
  CUDA.genKernel "reconstruct"
                 recs
                 (inG n :-> inG n)
                 
inG n = namedGlobal "apa" (variable "N") (2^n)
        :: Blocks (Array Pull (Exp Word32))
-} 
---------------------------------------------------------------------------
-- Testing WithCUDA aspects
---------------------------------------------------------------------------

testGRev :: Distrib (Array Pull EWord32)
            -> Distrib (Program (Array Pull EWord32))
testGRev = mapD (force . push . rev)


--wc1 = 
--  withCUDA $
--  do
--    -- Capture, compile and link the Obsidian program
--    -- into a CUDA function 
--myCudaFun <- capture testGRev (sizedGlobal (variable "N") 256) -- inputWord32
    
    -- Set up data and launch the kernel!
--    useVector (V.fromList [0..511::Word32]) $ \ inp -> 
--      allocaVector 512 $ \out ->
--        do
--          execute myCudaFun
--                  2  -- how many blocks 
--                  0   -- how much shared mem (will come from an analysis later) 
--                  inp out 
--          r <- lift$ CUDA.peekListArray 512 out
--          lift $ putStrLn $ show  (r :: [Word32])

 
--t2 =
--  do
--    let str = getScan 8
--    fp <- storeAndCompile "-arch=sm_30" "scan.cu" (header ++ str)
--    putStrLn fp
--    where
--      header = "#include <stdint.h>\n"

{- 
cs =
  withCUDA $
  do
    hist <- capture (hist 255) (sizedGlobal (variable "N") 256)  
    skl <- capture (sklanskyAllBlocks 8) (sizedGlobal (variable "N") 256)
    --constr <- capture (reconstruct 256)
    --                  ((sizedGlobal (variable "N") 256) :->
    --                   (sizedGlobal (variable "N") 256))
    constr <- capture recs
                      ((sizedGlobal (variable "N") 256) :->
                       (sizedGlobal (variable "N") 256))
                      
    
    useVector (V.fromList (P.replicate 10 (1::Word32) ++ [10..255])) $ \ inp -> 
      useVector (V.fromList (P.replicate 256 (0::Word32))) $ \tmp ->
        useVector (V.fromList (P.replicate 256 (0::Word32))) $ \ out -> 
          do
            -- Create histogram
            execute hist 1 0 inp tmp
            -- Run Scans
            execute skl
                    1  
                    (2*256*4)  
                    tmp tmp
            -- Reconstruct
            execute constr
                    1
                    0
                    (inp :-> tmp) out
                    
            r <- lift$ CUDA.peekListArray 256 out
            lift $ putStrLn $ show  (r :: [Word32])



wc2 = 
  withCUDA $
  do
    -- Capture, compile and link the Obsidian program
    -- into a CUDA function 
    myCudaFun <- capture (hist 256) (sizedGlobal (variable "N") 256)  
    
    -- Set up data and launch the kernel!
    useVector (V.fromList [0..511::Word32]) $ \ inp -> 
      allocaVector 512 $ \out ->
        do
          execute myCudaFun
                  2  -- how many blocks 
                  0   -- how much shared mem (will come from an analysis later) 
                  inp out 
          r <- lift$ CUDA.peekListArray 512 out
          lift $ putStrLn $ show  (r :: [Word32])


-} 

---------------------------------------------------------------------------
-- Experiments Nov 22 2012
---------------------------------------------------------------------------
{- 
mapB :: forall a b . Scalar b
        => (Array Pull a -> Program (Array Pull (Exp b))) -> 
        (Distrib a -> Program (Distrib (Exp b)))
mapB f inp@(Distrib nb bs bixf) =
  do
    name <- Allocate (bs * fromIntegral (sizeOf (undefined :: Exp b)))
                     (Pointer (typeOf (undefined :: Exp b)))
            

    return $ Distrib nb bs $ \bix tix -> index name tix
    where
      target name bix tix e = Assign name tix e
      arr = Array bs $ Pull $ \ix -> bixf
-} 


---------------------------------------------------------------------------
--
--  NEW PART
--
---------------------------------------------------------------------------


{-
   I want to think of mapD like this:

    f is a program on local data that potentially stores results
    in local memory (thats what the Program part in the return type means).

    mapD f takes an array distributed over a number of "blocks"
    (Not physically distributed at this time). The result on the
    other hand is potentially stored distributedly(wow thats really a word!)
    over the shared memories (thats the Program part inside of the Distrib).

   Questions: 
    Im not sure that it is impossible to missuse mapD to somehow affect
    elements outside of the "local" part of the array. How do I make sure so ?

   Thoughts: It should be possible to compose mapD's .. mapD f . mapD g.
    since there is no communication across blocks.

    Anything that actually communicates values across block boundaries
    should have the GlobArray return type instead of Distrib Something.
    (It should be impossible to go from a GlobArray  to a Distrib something
    inside a kernel) 
    
   

-} 

class LocalArrays a
instance LocalArrays (Array Pull a) 
instance LocalArrays (Array Push a) 
instance (LocalArrays a, LocalArrays b) => LocalArrays (a,b)
instance (LocalArrays a, LocalArrays b, LocalArrays c) => LocalArrays (a,b,c)
  

mapD :: (LocalArrays a, LocalArrays b) =>
        (a -> Program b) ->
        (Distrib a -> Distrib (Program b))
mapD f inp@(Distrib nb bixf) =
  Distrib nb $ \bid -> f (bixf bid)


toGlobArray :: Distrib (Program (Array Pull a)) -> GlobArray a
toGlobArray inp@(Distrib nb bixf) =
  GlobArray nb bs $     
    \wf bid tid ->
      do
        arr <- bixf bid 
        wf (arr ! tid) bid tid
  where
    -- Maybe not very efficient.. 
    bs = len $ fst $ runPrg 0 (bixf 0)


--  a post permutation (very little can be done with a GlobArray) 
permuteGlobal :: (Exp Word32 -> Exp Word32 -> (Exp Word32, Exp Word32))
                 -> Distrib (Array Pull a)
                 -> GlobArray a
permuteGlobal perm distr@(Distrib nb bixf) = 
  GlobArray nb bs $
    \wf bid tid ->
      do
        let (bid',tid') = perm bid tid
        wf ((bixf bid) ! tid) bid' tid' 
 where 
  bs = len (bixf 0)


gatherGlobal :: Distrib (Array Pull (Exp Word32))
                -> Exp Word32 -- expected output size number of blocks
                -> Word32     -- expected output size block-size
                -> Distrib (Array Pull a)
                -> GlobArray a
gatherGlobal indices@(Distrib nbs inf)
             nb bs
             elems@(Distrib ebs enf) =
  GlobArray nb bs $
   \wf bid tid ->
     let  inArr = inf bid
          inix  = inArr ! tid

          e     = (enf bid) ! tid 
     in wf e
           (inix `div` (fromIntegral bs))
           (inix `mod` (fromIntegral bs))
           
distribute :: Exp Word32 -> Word32 -> a -> Distrib (Array Pull a)
distribute nb bs e = Distrib nb $ \bid -> replicate bs e  

histogram :: Num a
             => Exp Word32
             -> Word32
             -> Distrib (Array Pull (Exp Word32))
             -> GlobArray a
histogram nb bs elems = gatherGlobal elems nb bs (distribute nb bs 1)


-- Im not sure this one is right. (Try to get to code generation soon) 
reconstruct inp@(Distrib nb bixf) pos@(Distrib _ posf) =
  permuteGlobal perm inp 
  where
    perm bix tix =
      let bs  = len (bixf bix) 
          gix = (bixf bix) ! tix
          bix' = gix `div` (fromIntegral bs)
          tix' = gix `mod` (fromIntegral bs)

          pgix = (posf bix') ! tix'
          pbix = pgix `div` (fromIntegral bs)
          ptix = pgix `mod` (fromIntegral bs) 
      in (pbix,ptix)


---------------------------------------------------------------------------
-- force a GlobArray
---------------------------------------------------------------------------
forceBT :: forall a. Scalar a => GlobArray (Exp a)
           -> Final (Program (Distrib (Array Pull (Exp a))))
forceBT (GlobArray nb bs pbt) = Final $ 
  do
      global <- Output $ Pointer (typeOf (undefined :: (Exp a)))
      
      ForAllBlocks nb 
        (\bid ->
          ForAll bs $ \ix -> 
            (pbt (assignTo global bs)) bid ix)
        
      return $ Distrib nb  $ 
        \bix -> Array bs (Pull (\ix -> index global ((bix * (fromIntegral bs)) + ix)))
    where 
      assignTo name s e b i = Assign name ((b*(fromIntegral s))+i) e


---------------------------------------------------------------------------
-- Experiment: reverse a GlobArray
---------------------------------------------------------------------------
reverseGA :: GlobArray (Exp Int32) -> GlobArray (Exp Int32) 
reverseGA (GlobArray nb bs pbt) =
  GlobArray nb bs $ \wf bid tid ->
      pbt wf (nb - 1 - bid)
             (fromIntegral bs - 1 - tid)
             



---------------------------------------------------------------------------
-- Get Launcher as text experiment. (For the windows users!) 
---------------------------------------------------------------------------


launcher1 =
  do
    myFun <- cudaCapture (forceBT . toGlobArray . sklanskyAllBlocks 3)
                         (sizedGlobal (variable "N") 256)

    d1 <- cudaUseVector (V.fromList [0..255::Int32]) Int32    
    d2 <- cudaAlloca 256 Int32

    cudaTime "Timing kernel execution: "
      $ do 
        cudaExecute myFun 1 2048 [d1] [d2] 
    
    return 10





---------------------------------------------------------------------------
-- Local Push array experiments
---------------------------------------------------------------------------

revP :: Pushable (Array p) => Array p a -> Array Push a
revP arr = Array n $Push $ \wf -> p (\(ix,a) -> wf (fromIntegral n - 1 - ix,a))
  where
    parr@(Array n (Push p)) = push arr

revP2 :: Array (PushP (Exp Word32 -> Program ())) a ->
         Array (PushP (Exp Word32 -> Program ())) a
revP2 (Array n (PushP p)) =
  Array n (PushP
           (\wf tid ->
             let wf' a ix = wf a (fromIntegral n - 1 - ix)  
             in p wf' tid))

push' (Array n (Pull ixf)) =
  Array n $
  PushP $ \wf tid -> wf (ixf tid) tid

revP2Test = printPrg (ForAll n $ \ix -> prg ix) 
  where prg = pfun (write n)
        (Array n (PushP pfun)) = revP2 (push' arr)
        write n a ix =
          Assign "bepa" ix a 
        arr = namedArray "apa" 256 :: Array Pull (Exp Int32)
        
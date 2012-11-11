{-# LANGUAGE FlexibleInstances, 
             FlexibleContexts,
             MultiParamTypeClasses,
             TypeOperators,
             TypeFamilies #-}

{- 
TODO TODO TODO
  # Everything in this module shoud be reimplemented!
    >>> IN PROGRESS <<< 

-} 

module Obsidian.GCDObsidian.CodeGen.InOut where 


--import Obsidian.GCDObsidian.Kernel
import Obsidian.GCDObsidian.Exp 
import Obsidian.GCDObsidian.Array
import Obsidian.GCDObsidian.Blocks
import Obsidian.GCDObsidian.Types
import Obsidian.GCDObsidian.Globs 
import Obsidian.GCDObsidian.Program
import Obsidian.GCDObsidian.ModifyArray
import qualified Obsidian.GCDObsidian.CodeGen.Program as CG 

import Prelude hiding (splitAt)
import Obsidian.GCDObsidian.Library (splitAt,concP, unzipp,zipp)


import Control.Monad.State

import Data.Word
------------------------------------------------------------------------------
-- Block index ! 
bid :: Exp Word32
bid = variable "blockIdx.x" 

-- should work for any indexible
cTypeOfArray :: Scalar a =>  Array Pull (Exp a) -> Type 
cTypeOfArray arr = Pointer (typeOf (arr ! variable "X"))

--cTypeOfGlobalArray :: Scalar a =>  GlobalArray Pull (Exp a) -> Type 
--cTypeOfGlobalArray (GlobalArray _ arr) = Pointer (typeOf (pullFun arr (variable "X")))

globalTarget :: Scalar a => Name
                -> Exp Word32
                -> (Exp Word32, Exp a) -> Program ()
globalTarget nom blockSize (i,a) = Assign nom ((bid * blockSize) + i)  a 

--globalTarget :: Scalar a => Name
--                -> Exp Word32
--               -> (Exp Word32, Exp a) -> NameSupply (Program ())
--globalTarget nom blockSize (i,a) =
--  return $ Assign nom ((bid * blockSize) + i)  a 


--globalTargetAgain :: Scalar a => Name -> (Exp Word32, Exp a) -> NameSupply (Program ()) 
--globalTargetAgain nom (i,a) = return $ Assign nom i a 

class BasePush a where 
  cType :: Array p (Exp a) -> Type 
  --cTypeGlob :: GlobalArray p (Exp a) -> Type
  
instance BasePush (Int) where 
  cType arr = Pointer Int 
  ---cTypeGlob arr = Pointer Int
  
instance BasePush (Float) where 
  cType arr = Pointer Float
  --cTypeGlob arr = Pointer Float

instance BasePush (Word32) where 
  cType arr = Pointer Word32
  --cTypeGlob arr = Pointer Word32


-----------------------------------------------------------------------------
-- Inputs Outputs 

class InOut a where 
  --                                Name   Type Size
  createInputs :: a -> State (Int,[(String,Type,Word32)]) a 
  
  writeOutputs :: NumThreads -> 
                  a -> 
            --      e -> 
                  State (Int,[(String,Type,Word32)]) (Program ())
  
  -- is this a hack ? yes, and poorly named!
  gcdThreads :: a -> Word32

newInOut name t s = do 
  (i,ins) <- get 
  let newname = name ++ show i
  put (i+1,(newname,t,s):ins) 
  return newname
  
newGlobalInputArray t = do 
  (i,ins) <- get 
  let newArrayName = "input" ++ show i
      newArrayLen  = "n" ++ show i 
  put (i+1,(newArrayLen,Word32,undefined):
           (newArrayName,t,undefined):
           ins)
  return (newArrayName,newArrayLen)

runInOut :: State (Int,[(String,Type,Word32)]) a 
            -> (Int,[(String,Type,Word32)]) 
            -> (a,[(String,Type,Word32)]) 
runInOut f s = (a,reverse xs)
  where 
    (a,(_,xs)) = runState f s

runInOut_ :: State (Int,[(String,Type,Word32)]) a 
            -> (a,[(String,Type,Word32)]) 
runInOut_ f = (a,reverse xs)
  where 
    (a,(_,xs)) = runState f (0,[])


instance Scalar a => InOut (Array Pull (Exp a)) where
  createInputs arr  = do 
    name <- newInOut "input" (cTypeOfArray arr) (len arr)
    let n = fromIntegral (len arr) 
    return$ Array (len arr) (Pull (\ix -> index name (bid * n + ix)))
    
  writeOutputs threadBudget arr {-e-} = do   
    
    name <- newInOut "result" (cTypeOfArray arr) (len arr)
    
    if ( len arr <= threadBudget ) 
      then do 
         let (Array n (Push parr)) = push arr
         --return$ SyncUnit (len arr) {-threadBudget-}  
         --  (pushApp parr (targetArray  name)) e
         return$ parr (globalTarget name (fromIntegral (len arr)))
      else do 
         let n  = len arr
             tb = threadBudget 
             tbInN =  n `div` tb 
             rest  = n `rem` tb
             sp = tbInN * tb
             (a1,a2) = splitAt sp arr
             
             -- Change to push' for (tid*2,tid*2+1) scheme 
             -- Fix this so that the switch is easier. 
             pa1     = push'' tbInN a1
             pa2     = push a2
             
             (Array _ (Push parr)) = if (rest == 0) 
               then pa1 
               else concP (pa1,pa2)
         
         --return$ SyncUnit threadBudget (pushApp parr (targetArray  name)) e
         return$ parr (globalTarget name (fromIntegral (len arr))) -- (targetArray  name)
         
         
                     
  gcdThreads arr = len arr
    

instance (BasePush a, Scalar a) => InOut (Array Push (Exp a)) where
  createInputs arr  = do 
    name <- newInOut "input" (cType arr) (len arr)
    let n = fromIntegral (len arr) 
    return$ push$ Array (len arr)
                        (Pull (\ix -> index name (bid * n + ix)))  
    
  writeOutputs threadBudget parr@(Array _ (Push pfun)) = do   
    
    name <- newInOut "result" (cType parr) (len parr)
   
    return$ pfun (globalTarget name (fromIntegral (len parr))) 
  
         
   -- HACK HACK HACK    
  gcdThreads (Array n (Push parr)) = programThreads prg
    where prg = parr (globalTarget "dummy" (fromIntegral n)) 


instance (InOut a, InOut b) => InOut (a, b) where 
  createInputs (a0,a1)  =     
    do 
      a0' <- createInputs a0 
      a1' <- createInputs a1 
      return (a0',a1')
  
  writeOutputs threadBudget (a0,a1) {-e-}= do   
    
    --(SyncUnit nt1 prg1 e1) <- writeOutputs threadBudget a0 e
    --(SyncUnit nt2 prg2 e2) <- writeOutputs threadBudget a1 e
    prg1 <- writeOutputs threadBudget a0 {-e-}
    prg2 <- writeOutputs threadBudget a1 {-e-}
    --error ( show nt1 ++ " " ++ show nt2 ++ " " ++ show threadBudget)
    return$ prg1 *>* prg2  -- syncUnitFuseGCD s0 s1
   
   -- return$ SyncUnit (max nt1 nt2)  -- what exactly should this be
   --                   (prg1 *>* prg2) e1 -- syncUnitFuseGCD s0 s1
   
  gcdThreads (a0,a1) = gcd (gcdThreads a0) (gcdThreads a1)
  
instance (InOut (Array Pull a), InOut (Array Pull b)) => InOut (Array Pull (a,b)) where
  createInputs arr = 
    do 
      let (a0,a1) = unzipp arr 
      a0' <- createInputs a0
      a1' <- createInputs a1
      return (zipp (a0',a1'))
  writeOutputs threads arr = 
    do 
      let (a0,a1) = unzipp arr
      prg1 <- writeOutputs threads a0
      prg2 <- writeOutputs threads a1 
      return$ prg1 *>* prg2
      
  gcdThreads arr = 
    let (a0,a1) = unzipp arr
    in  gcd (gcdThreads a0) (gcdThreads a1)
        
---------------------------------------------------------------------------
-- New approach (hopefully)
---------------------------------------------------------------------------
-- "reify" Haskell functions into CG.Programs

{-
   Blocks needs to be of specific sizes (a design choice we've made).
   Because of this a prototypical input array needs to be provided
   that has a static block size (the number of blocks is dynamic).

   To make things somewhat general a heterogeneous list of input arrays
   that has same shape as the actual parameter list of the function
   is passed into toProgram (the reifyer). 

-} 
  

type Inputs = [(Name,Type)] 

class ToProgram a b where
  toProgram :: Int -> (a -> b) -> Ips a b -> (Inputs,CG.Program ())

instance ToProgram (Blocks (Array Pull (Exp Int))) (Program a) where
  toProgram i f (Blocks n blkf)  = ([(nom,Pointer Int)],CG.runPrg (f input))
    where
      nom = "input" ++ show i
      var = "N" ++ show i
      n   = len (blkf (variable "X")) 
      input = namedGlobal  nom (variable var) n
      
instance ToProgram (Blocks (Array Pull (Exp Word32))) (Program a) where
  toProgram i f (Blocks n blkf)  = ([(nom,Pointer Int)],CG.runPrg (f input))
    where
      nom = "input" ++ show i
      var = "N" ++ show i
      n   = len (blkf (variable "X")) 
      input = namedGlobal  nom (variable var) n      

instance ToProgram b c =>
         ToProgram (Blocks (Array Pull (Exp Int))) (b -> c) where
  toProgram i f ((Blocks n blkf) :-> rest) = ((nom,Pointer Int):ins,prg)
    where
      (ins,prg) = toProgram (i+1) (f input) rest
      nom = "input" ++ show i
      var = "N" ++ show i
      n   = len (blkf (variable "X")) 
      input = namedGlobal  nom (variable var) n

instance ToProgram b c =>
         ToProgram (Blocks (Array Pull (Exp Word32))) (b -> c) where
  toProgram i f ((Blocks n blkf) :-> rest) = ((nom,Pointer Int):ins,prg)
    where
      (ins,prg) = toProgram (i+1) (f input) rest
      nom = "input" ++ show i
      var = "N" ++ show i
      n   = len (blkf (variable "X")) 
      input = namedGlobal  nom (variable var) n

           
      
---------------------------------------------------------------------------
-- heterogeneous lists of inputs 
---------------------------------------------------------------------------
data head :-> tail = head :-> tail

infixr 5 :->

type family Ips a b
type family Ips' a 

type instance Ips' (Blocks (Array Pull (Exp Int))) =
  Blocks (Array Pull (Exp Int))
type instance Ips' (Blocks (Array Pull (Exp Word32))) =
  Blocks (Array Pull (Exp Word32))

type instance Ips a (Program b) = Ips' a
type instance Ips a (b -> c) =  Ips' a :-> Ips b c


{- TODO:
    What about Blocks (Array p1 a1, Array p2 a2)
     (blocks of pairs of arrays) -- limit what can live inside a block  ? 


-} 
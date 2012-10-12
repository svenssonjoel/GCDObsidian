{-# LANGUAGE FlexibleInstances, 
             FlexibleContexts,
             MultiParamTypeClasses#-}

{- 
  DONE: use a splitAt in the writeOutputs when 
        The output array is larger than the threadbudget
        and use two or many more levels of writes. (Possibly) 


-} 

module Obsidian.GCDObsidian.CodeGen.InOut where 


import Obsidian.GCDObsidian.Kernel
import Obsidian.GCDObsidian.Exp 

import Obsidian.GCDObsidian.Types
import Obsidian.GCDObsidian.Globs 
import Obsidian.GCDObsidian.Program

import Obsidian.GCDObsidian.ModifyArray
import Obsidian.GCDObsidian.Array 
import Obsidian.GCDObsidian.Shape

import Prelude hiding (splitAt)
import Obsidian.GCDObsidian.Library hiding (reverse) -- (splitAt,concP,unzipp,zipp)


import Control.Monad.State

import Data.Word
------------------------------------------------------------------------------
-- Block index ! 
bid :: Exp Word32
bid = variable "blockIdx.x" 

-- should work for any indexible
cTypeOfArray :: Scalar a =>  ArrayPull DIM1 (Exp a) -> Type 
cTypeOfArray arr = Pointer $ typeOf (arr ! ix)
  where ix = mkIndex (pullShape arr) [variable "X"] 

cTypeOfPullG :: Scalar a => PullG DIM1 DIM1 (Exp a) -> Type
cTypeOfPullG arr = Pointer $ typeOf $ arr !* (bix,tix)
  where
    bix = mkIndex (pullGGridDim arr) [variable "b"]
    tix = mkIndex (pullGBlockDim arr) [variable "t"] 

--cTypeOfGlobalArray :: Scalar a =>  GlobalArray Pull (Exp a) -> Type 
--cTypeOfGlobalArray (GlobalArray arr _) = Pointer (typeOf (pullFun arr (variable "X")))

globalTarget :: Scalar a => Name -> Exp Word32 -> (Exp Word32, Exp a) -> Program ()
globalTarget nom blockSize (i,a) = Assign nom ((bid * blockSize) + i)  a 

-- TODO: This is not right of course !
globalTarget' :: Scalar a => Name -> Exp Word32 -> Shape sh Word32 -> (Shape (E sh) (Exp Word32),Exp a) -> Program ()
globalTarget' nom blockSize sh (ix,a) = Assign nom ((bid * blockSize) + toIndex sh ix) a
                                                   
newGlobalTarget nom gsh bsh (bix,tix,a) =
  Assign nom ((toIndex gsh bix * blockSize) + toIndex bsh tix) a
  where
    blockSize = fromIntegral (size bsh)

globalTargetAgain :: Scalar a => Name -> (Exp Word32, Exp a) -> Program () 
globalTargetAgain nom (i,a) = Assign nom i a 

class CTypeable a e where 
    cType :: a (Exp e) -> Type 

instance CTypeable (Push sh) Int where 
    cType arr = Pointer Int 
                             
instance CTypeable (Push sh) Float where 
    cType arr = Pointer Float 

instance CTypeable (Push sh) Word32 where 
    cType arr = Pointer Word32

{- 
class BasePush a where 
  cType :: Array p (Exp a) -> Type 
  --cTypeGlob :: GlobalArray p (Exp a) -> Type
  
instance BasePush (Int) where 
  cType arr = Pointer Int 
  --cTypeGlob arr = Pointer Int
  
instance BasePush (Float) where 
  cType arr = Pointer Float
  --cTypeGlob arr = Pointer Float

instance BasePush (Word32) where 
  cType arr = Pointer Word32
  --cTypeGlob arr = Pointer Word32
-} 

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


instance Scalar a => InOut (Pull DIM1 (Exp a)) where
  createInputs arr  = do 
    name <- newInOut "input" (cTypeOfArray arr) numElt
    let n = fromIntegral numElt  -- total needed threads (elements) 
    return $ Pull psh 
                  (\ix -> index name (bid * n * (toIndex psh ix)))
    -- TODO: Think more about this. Maybe only GlobalArrays can be 
    --       inputs or outputs ! 
    --return$ Array (len arr) (Pull (\ix -> index name (bid * n + ix)))  
    where
      psh    = pullShape arr
      numElt = size psh
  writeOutputs threadBudget arr {-e-} = do   
    
    name <- newInOut "result" (cTypeOfArray arr) numElt    
    if ( numElt <= threadBudget ) 
      then do 
         let (Push n parr) = toPush arr
         --return$ SyncUnit (len arr) {-threadBudget-}  
         --  (pushApp parr (targetArray  name)) e
         return $ (unP parr) (globalTarget' name (fromIntegral (size n)) n)  
      else error "REMOVED THIS CASE TEMPORARILY" 
    where
     numElt = size (pullShape arr)      
      
           
          {- 
          do 
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
         return$ (unP parr) (globalTarget name (fromIntegral (len arr))) -- (targetArray  name)
         -} 
         
                     
  gcdThreads (Pull n _)  = size n -- len arr

-- Global Pull array instance ---
instance Scalar a => InOut (PullG DIM1 DIM1 (Exp a)) where
  createInputs arr  = do 
    name <- newInOut "input" (cTypeOfPullG arr) numElt
    let n = fromIntegral numElt  -- total needed threads (elements) 
    return $ PullG gsh bsh 
                   (\bix tix -> indexG name
                                       (toIndex gsh bix)
                                       (toIndex bsh tix))

    -- TODO: Think more about this. Maybe only GlobalArrays can be 
    --       inputs or outputs ! 
    --return$ Array (len arr) (Pull (\ix -> index name (bid * n + ix)))  
    where
      -- psh    = pullShape arr
      gsh    = pullGGridDim arr 
      bsh    = pullGBlockDim arr
      numElt = size gsh * size bsh 
      
  writeOutputs threadBudget arr {-e-} = do   
    
    name <- newInOut "result" (cTypeOfPullG arr) numElt    
    if ( threadsPerBlock <= threadBudget ) 
      then do 
         let (PushG gsh bsh parr) = toPushG arr
         --return$ SyncUnit (len arr) {-threadBudget-}  
         --  (pushApp parr (targetArray  name)) e
         return $ (unP parr)
                  (newGlobalTarget name gsh bsh)
           -- (globalTarget' name (fromIntegral (size n)) n)  
      else error "REMOVED THIS CASE TEMPORARILY" 
    where
      -- total number of elements 
     numElt = size (pullGGridDim arr) *
              size (pullGBlockDim arr)
     threadsPerBlock = size (pullGBlockDim arr) 
      
           
          {- 
          do 
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
         return$ (unP parr) (globalTarget name (fromIntegral (len arr))) -- (targetArray  name)
         -} 
         
  -- This is getting fuzzy.
  -- This entire file needs to be remade and cleared up.
  gcdThreads (PullG gsh bsh _)  = size bsh -- len arr



instance (CTypeable (Push DIM1) a, Scalar a) => InOut (Push DIM1 (Exp a)) where
  createInputs arr = do 
    name <- newInOut "input" (cType arr) (size (pushShape arr))
    let n = fromIntegral (size psh) 
    return$ toPush$ Pull psh (\ix -> index name (bid * n + toIndex psh ix))  
      where
        psh = pushShape arr
  writeOutputs threadBudget parr@(Push sh pfun) = do   
    
    name <- newInOut "result" (cType parr) (size sh)
   
    return$ (unP pfun) (globalTarget' name (fromIntegral (size sh)) sh )
  
         
   -- HACK HACK HACK    
  gcdThreads (Push sh parr) = programThreads prg
    where prg = (unP parr)  (globalTarget' "dummy" (fromIntegral (size sh)) sh) 

{- 
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
-}
 {- 
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
    -}
{-           
instance (BasePush a, Scalar a) => InOut (Array Modify (Exp a)) where
  createInputs arr  = error "Modify arrays cannot be inputs"
    
  writeOutputs threadBudget parr@(Array _ (Modify pfun op)) = do   
    
    name <- newInOut "result" (cType parr) (len parr)
   
    return$ (unP pfun) (globalTargetModify op name (fromIntegral (len parr))) 
  
         
   -- HACK HACK HACK    
  gcdThreads (Array n (Modify parr op)) = programThreads prg
    where prg = (unP parr) (globalTargetModify op "dummy" (fromIntegral n)) 

globalTargetModify :: Scalar a => Atomic (Exp a) -> Name -> Exp Word32 -> Exp Word32 -> Program ()
globalTargetModify op nom blockSize i =
  AtomicOp "dummy" nom ((bid * blockSize) + i) op
-}
        
--------------------------------------------------------------------------        
-- New approach to input output
  
        
class GlobalInput a where
  createGlobalInput :: a -> State (Int,[(String,Type,Word32)]) a   
  
class GlobalOutput a where 
  writeGlobalOutput :: NumThreads -> 
                       a -> 
                       State (Int,[(String,Type,Word32)]) (Program ())
   
                       
                       
--------------------------------------------------------------------------                       
-- Base

{-   
instance Scalar a => GlobalInput (GlobalArray Pull (Exp a)) where 
  createGlobalInput arr@(GlobalArray _ _) = do 
    (name,n) <- newGlobalInputArray (cTypeOfGlobalArray arr)
    let fun ix = index name ix 
    return$ GlobalArray (Pull fun) (variable n)
      
instance Scalar a => GlobalInput (Exp a) where   
  createGlobalInput a = do 
    name <- newInOut "v" (typeOf a) undefined {- again no size -} 
    return (variable name)
      
instance (GlobalInput a, GlobalInput b) 
         => GlobalInput (a,b) where 
  createGlobalInput (a,b) = do 
    a' <- createGlobalInput a
    b' <- createGlobalInput b
    return (a',b')
      
instance (BasePush a, Scalar a) => GlobalOutput (GlobalArray Push (Exp a)) where       
  writeGlobalOutput threadBugdet parr@(GlobalArray (Push pfun) n) = do  
    name <- newInOut "result" (cTypeGlob parr) undefined 
    return$ pfun (globalTargetAgain name)
    
-}
--------------------------------------------------------------------------
-- complex inputs 

-- TODO!
{-# LANGUAGE FlexibleInstances, 
             FlexibleContexts#-}

module Obsidian.GCDObsidian.CodeGen.InOut where 


import Obsidian.GCDObsidian.Kernel
import Obsidian.GCDObsidian.Exp 
import Obsidian.GCDObsidian.Array 


import Control.Monad.State

import Data.Word
------------------------------------------------------------------------------
-- Block index ! 
bid :: Exp Word32
bid = variable "bid" 


cTypeOfArray :: Scalar a =>  Array (Exp a) -> Type 
cTypeOfArray arr = Pointer (typeOf (arr ! variable "X"))


-----------------------------------------------------------------------------
-- Inputs Outputs 

class InOut a where 
  --                                Name   Type Size
  createInputs :: a -> State (Int,[(String,Type,Word32)]) a 
  
  writeOutputs :: NumThreads -> 
                  a -> 
                  e -> 
                  State (Int,[(String,Type,Word32)]) (SyncUnit e) 
  writeOutputsP :: NumThreads -> 
                   a -> 
                   e -> 
                   State (Int,[(String,Type,Word32)]) (PSyncUnit e)
  
  -- is this a hack ?
  gcdThreads :: a -> Word32

newInOut name t s = do 
  (i,ins) <- get 
  let newname = name ++ show i
  put (i+1,(newname,t,s):ins) 
  return newname

runInOut :: State (Int,[(String,Type,Word32)]) a 
            -> (Int,[(String,Type,Word32)]) 
            -> (a,[(String,Type,Word32)]) 
runInOut f s = (a,reverse xs)
  where 
    (a,(_,xs)) = runState f s

instance (OArray Array a, Scalar a) => InOut (Array (Exp a)) where
  createInputs arr  = do 
    name <- newInOut "input" (cTypeOfArray arr) (len arr)
    let n = fromIntegral (len arr) 
    return$ Array (\ix -> index name (bid * n + ix))  (len arr)
    
  writeOutputs threadBudget arr e = do   
    let llArr  = toLL arr 
        arrLen = staticLength llArr
    
    name <- newInOut "result" (cTypeOfArray arr) arrLen
    
    let targ   = \ix -> (bid * (fromIntegral arrLen) + ix) 
        maxGCD = maximum [gcd arrLen i| i <- [1..threadBudget]]
    return$ SyncUnit maxGCD (StoreListCons (Store name arrLen [Write targ llArr e])
                             StoreListNil)
    
  writeOutputsP threadBudget arr e = do   
    
    name <- newInOut "result" (cTypeOfArray arr) (len arr)
    
    let maxGCD = maximum [gcd (len arr) i| i <- [1..threadBudget]]
        parr = toArrayP arr
    return$ PSyncUnit maxGCD [pushApp parr (targetArray  name)] e
    
                     
  gcdThreads arr = len arr
    
instance (InOut a, InOut b) => InOut (a, b) where 
  createInputs (a0,a1)  =     
    do 
      a0' <- createInputs a0 
      a1' <- createInputs a1 
      return (a0',a1')
  
  writeOutputs threadBudget (a0,a1) e = do   
    s0 <- writeOutputs threadBudget a0 e
    s1 <- writeOutputs threadBudget a1 e
    
    return$ syncUnitFuseGCD s0 s1
  writeOutputsP threadBudget (a0,a1) e = do   
    (PSyncUnit nt1 prgs1 e1) <- writeOutputsP threadBudget a0 e
    (PSyncUnit nt2 prgs2 e2) <- writeOutputsP threadBudget a1 e
    
    return$ PSyncUnit (gcd nt1 nt2)  
                      (prgs1 ++ prgs2) e1 -- syncUnitFuseGCD s0 s1
   
  gcdThreads (a0,a1) = gcd (gcdThreads a0) (gcdThreads a1)
  
  
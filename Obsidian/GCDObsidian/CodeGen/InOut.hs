{-# LANGUAGE FlexibleInstances, 
             FlexibleContexts#-}

{- 
  TODO: use a splitAt in the writeOutputs when 
        The output array is larger than the threadbudget
        and use two or many more levels of writes. (Possibly) 


-} 

module Obsidian.GCDObsidian.CodeGen.InOut where 


import Obsidian.GCDObsidian.Kernel
import Obsidian.GCDObsidian.Exp 
import Obsidian.GCDObsidian.Array 
import Obsidian.GCDObsidian.Types
import Obsidian.GCDObsidian.Globs 

import Prelude hiding (splitAt)
import Obsidian.GCDObsidian.Library (splitAt,concP)


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

instance Scalar a => InOut (Array (Exp a)) where
  createInputs arr  = do 
    name <- newInOut "input" (cTypeOfArray arr) (len arr)
    let n = fromIntegral (len arr) 
    return$ Array (\ix -> index name (bid * n + ix))  (len arr)
    
  writeOutputs threadBudget arr e = do   
    
    name <- newInOut "result" (cTypeOfArray arr) (len arr)
    
    if ( len arr <= threadBudget ) 
      then do 
         let parr = push arr
         return$ SyncUnit threadBudget  [pushApp parr (targetArray  name)] e
      else do 
         let n  = len arr
             tb = threadBudget 
             tbInN =  n `div` tb 
             rest  = n `rem` tb
             sp = tbInN * tb
             (a1,a2) = splitAt sp arr
             pa1     = push' tbInN a1
             pa2     = push a2
             
             parr = if (rest == 0) 
               then pa1 
               else concP pa1 pa2
         
         return$ SyncUnit threadBudget  [pushApp parr (targetArray  name)] e
         
                     
  gcdThreads arr = len arr
    
instance (InOut a, InOut b) => InOut (a, b) where 
  createInputs (a0,a1)  =     
    do 
      a0' <- createInputs a0 
      a1' <- createInputs a1 
      return (a0',a1')
  
  writeOutputs threadBudget (a0,a1) e = do   
    
    (SyncUnit nt1 prgs1 e1) <- writeOutputs threadBudget a0 e
    (SyncUnit nt2 prgs2 e2) <- writeOutputs threadBudget a1 e
    --error ( show nt1 ++ " " ++ show nt2 ++ " " ++ show threadBudget)
    return$ SyncUnit (max nt1 nt2)  -- what exactly should this be
                      (prgs1 ++ prgs2) e1 -- syncUnitFuseGCD s0 s1
   
  gcdThreads (a0,a1) = gcd (gcdThreads a0) (gcdThreads a1)
  
  
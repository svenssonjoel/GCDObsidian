

module Obsidian.GCDObsidian.CodeGen.InOut where 


import Obsidian.GCDObsidian.Kernel
import Obsidian.GCDObsidian.Exp 
import Obsidian.GCDObsidian.Array 


import Control.Monad.State

------------------------------------------------------------------------------
-- Block index ! 
bid :: Exp Int
bid = variable "bid" 


cTypeOfArray :: Scalar a =>  Array a -> Type 
cTypeOfArray arr = Pointer (typeOf (arr ! variable "X")) 


-----------------------------------------------------------------------------
-- Inputs Outputs 

class InOut a where 
  createInputs :: a -> State (Int,[(String,Type)]) a 
  
  writeOutputs :: NumThreads -> 
                  a -> 
                  e -> 
                  State (Int,[(String,Type)]) (Code e) 
  
  -- is this a hack ?
  gcdThreads :: a -> Int 

newInOut name t = do 
  (i,ins) <- get 
  let newname = name ++ show i
  put (i+1,(newname,t):ins) 
  return newname

runInOut :: State (Int,[(String,Type)]) a 
            -> (Int,[(String,Type)]) 
            -> (a,[(String,Type)]) 
runInOut f s = (a,reverse xs)
  where 
    (a,(_,xs)) = runState f s

instance Scalar a => InOut (Array a) where
  createInputs arr  = do 
    name <- newInOut "input" (cTypeOfArray arr) -- (typeOf (arr ! (variable "X")))
    let n = fromIntegral (len arr) 
    return$ Array (\ix -> index name (bid * n + ix))  (len arr)
    
  writeOutputs threadBudget arr e = do   
    name <- newInOut "result" (cTypeOfArray arr) -- (typeOf (arr ! (variable "X"))) 
    
    let llArr  = toLL arr 
        arrLen = staticLength llArr
        targ   = \ix -> index name (bid * (fromIntegral arrLen) + ix) 
        maxGCD = maximum [gcd arrLen i| i <- [1..threadBudget]]
    return$ code$ Store maxGCD [Write targ llArr e] 
    
    
  gcdThreads arr = len arr
    
instance (InOut a, InOut b) => InOut (a, b) where 
  createInputs (a0,a1)  =     
    do 
      a0' <- createInputs a0 
      a1' <- createInputs a1 
      return (a0',a1')
  
  writeOutputs threadBudget (a0,a1) e = do   
    c0 <- writeOutputs threadBudget a0 e
    c1 <- writeOutputs threadBudget a1 e
    
    return$ c0 +++ c1
   
  gcdThreads (a0,a1) = gcd (gcdThreads a0) (gcdThreads a1)
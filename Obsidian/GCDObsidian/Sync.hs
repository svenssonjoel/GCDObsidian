
module Obsidian.GCDObsidian.Sync where 

import Obsidian.GCDObsidian.Kernel
import Obsidian.GCDObsidian.Exp
import Obsidian.GCDObsidian.Array

import Control.Monad.Writer

------------------------------------------------------------------------------
-- Syncs in the new setting 

sync :: Scalar a => Array (Exp a) -> Kernel (Array (Exp a))
sync = pSyncArray

-- Work on the Scalar a thing!!!y
pSyncArray  :: Scalar a => Array (Exp a) -> Kernel (Array (Exp a))
pSyncArray arr = 
  do 
    name <- newArray
    
    tell$ Seq (syncUnit (len arr) 
                [Allocate name (es * (len arr)) t 
                  (pushApp parr (targetArray name))]) Skip
            
    return (Array (index name) (len arr))
      
  where 
    es = fromIntegral$ sizeOf (arr ! 0) 
    t  = Pointer$ Local$ typeOf (arr ! 0)

    parr = toArrayP arr 
        

-- THE GCD THING
pSyncArrays :: (Scalar a, Scalar b) => (Array (Exp a),Array (Exp b)) -> Kernel (Array (Exp a), Array (Exp b))
pSyncArrays (a1,a2) = 
  do 
    name1 <- newArray 
    name2 <- newArray 
    
    tell$ Seq (syncUnit n [Allocate name1 (es1 * (len a1)) t1
                             (pushApp pa1 (targetArray name1))
                            ,Allocate name2 (es2 * (len a2)) t2
                             (pushApp pa2 (targetArray name2))]) Skip
            
    return (Array (index name1) (len a1)
           ,Array (index name2) (len a2))
      
  where  
    es1 = fromIntegral$ sizeOf (a1 ! 0) 
    es2 = fromIntegral$ sizeOf (a2 ! 0) 
    t1  = Pointer$ Local$ typeOf (a1 ! 0)
    t2  = Pointer$ Local$ typeOf (a2 ! 0)
    n   = gcd (len a1) (len a2) 
    pa1 = toArrayP a1 
    pa2 = toArrayP a2    

    
pSyncArrayP :: Scalar a => ArrayP (Exp a) -> Kernel (Array (Exp a)) 
pSyncArrayP arr@(ArrayP func n)  = 
  do 
    name <- newArray
    
    let result = Array (index name) n         
        es = fromIntegral$ sizeOf (result ! 0) 
        t  = Pointer$ Local$ typeOf (result ! 0)


    tell$ Seq (syncUnit n 
               [Allocate name (es * n) t 
                (pushApp arr (targetArray name))]) Skip
    return result

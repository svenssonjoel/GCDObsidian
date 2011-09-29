{-# LANGUAGE FlexibleInstances, 
             FlexibleContexts, 
             MultiParamTypeClasses  #-}  
module Obsidian.GCDObsidian.Sync where 

import Obsidian.GCDObsidian.Kernel
import Obsidian.GCDObsidian.Exp
import Obsidian.GCDObsidian.Array
import Obsidian.GCDObsidian.Types
import Obsidian.GCDObsidian.Program
import Obsidian.GCDObsidian.Library
import Obsidian.GCDObsidian.Elem


import Control.Monad.Writer
import Data.Word

------------------------------------------------------------------------------
-- Syncs in the new setting 

class Syncable arr a where 
  sync ::  arr a -> Kernel (Array a )

instance (Pushy arr, Scalar a) => Syncable arr (Exp a) where 
  sync = pSyncA

instance (Syncable Array a, Syncable Array b) => Syncable Array (a,b) where
  sync arr = do
    a1' <- sync a1
    a2' <- sync a2
    return$ zipp (a1',a2')
    where 
      (a1,a2) = unzipp arr

instance (Syncable Array a, Syncable Array b, Syncable Array c) 
         => Syncable Array (a,b,c) where
  sync arr = do 
    a1' <- sync a1 
    a2' <- sync a2 
    a3' <- sync a3
    return$ zipp3 (a1',a2',a3')
    where 
      (a1,a2,a3) = unzipp3 arr


composeS [] = pure id
composeS (f:fs) = f ->- sync ->- composeS fs


pSyncA :: (Scalar a, Pushy arr) 
          => arr (Exp a) -> Kernel (Array (Exp a)) 
pSyncA arrIn = 
  do 
    name <- newArray
    
    let result = Array (index name) n         
        es = fromIntegral$ sizeOf (result ! 0) 
        t  = Pointer$ Local$ typeOf (result ! 0)
        p  = pushApp arr (targetArray name)

    tell$ Seq (syncUnit (programThreads p) 
               (Allocate name (es * n) t 
                p)) Skip
    return result
  where 
    arr@(ArrayP func n) = push arrIn 


pSyncAP :: Scalar a  
        => Word32 -> Array (Exp a) -> Kernel (Array (Exp a)) 
pSyncAP elemsPT arrIn = sync arr'
  where 
    arr' = push' elemsPT arrIn 
    


-- Work on the Scalar a thing!!!
pSyncArray  :: Scalar a => Array (Exp a) -> Kernel (Array (Exp a))
pSyncArray arr = 
  do 
    name <- newArray
    
    let p = pushApp parr (targetArray name)
        
    tell$ Seq (syncUnit  (programThreads p) ---(len arr) 
                (Allocate name (es * (len arr)) t 
                  p)) Skip
            
    return (Array (index name) (len arr))
      
  where 
    es = fromIntegral$ sizeOf (arr ! 0) 
    t  = Pointer$ Local$ typeOf (arr ! 0)

    parr = push arr 
        

-- THE GCD THING
pSyncArrays :: (Scalar a, Scalar b) => (Array (Exp a),Array (Exp b)) -> Kernel (Array (Exp a), Array (Exp b))
pSyncArrays (a1,a2) = 
  do 
    name1 <- newArray 
    name2 <- newArray 
    
    tell$ Seq (syncUnit n (Allocate name1 (es1 * (len a1)) t1
                             (pushApp pa1 (targetArray name1))
                             *>*
                             Allocate name2 (es2 * (len a2)) t2
                              (pushApp pa2 (targetArray name2)))) Skip
            
    return (Array (index name1) (len a1)
           ,Array (index name2) (len a2))
      
  where  
    es1 = fromIntegral$ sizeOf (a1 ! 0) 
    es2 = fromIntegral$ sizeOf (a2 ! 0) 
    t1  = Pointer$ Local$ typeOf (a1 ! 0)
    t2  = Pointer$ Local$ typeOf (a2 ! 0)
    n   = gcd (len a1) (len a2) 
    pa1 = push' w1 a1 
    pa2 = push' w2 a2    
    (w1,w2) = nWrites n (pa1,pa2)
    
nWrites m (p1@(ArrayP _ n1),p2@(ArrayP _ n2)) = (p1Writes, p2Writes)
  where 
    p1Writes = n1 `div` m
    p2Writes = n2 `div` m
    
pSyncArrayP :: Scalar a => ArrayP (Exp a) -> Kernel (Array (Exp a)) 
pSyncArrayP arr@(ArrayP func n)  = 
  do 
    name <- newArray
    
    let result = Array (index name) n         
        es = fromIntegral$ sizeOf (result ! 0) 
        t  = Pointer$ Local$ typeOf (result ! 0)
        p  = pushApp arr (targetArray name)

    tell$ Seq (syncUnit (programThreads p) 
               (Allocate name (es * n) t 
                p)) Skip
    return result



pSyncArrayP2 :: (Scalar a, Scalar b ) => ArrayP (Exp a,Exp b) -> Kernel (Array (Exp a,Exp b))
pSyncArrayP2 arr@(ArrayP f n) =  
  do 
    name1 <- newArray
    name2 <- newArray
                      
    let result1 = Array (index name1) n
        result2 = Array (index name2) n
        t1 = Pointer$ Local$ typeOf (result1 ! 0) 
        t2 = Pointer$ Local$ typeOf (result2 ! 0)
        es1 = fromIntegral$ sizeOf (result1 ! 0)
        es2 = fromIntegral$ sizeOf (result2 ! 0)
        p = pushApp arr (targetPair name1 name2)
        
    tell$ Seq (syncUnit (programThreads p) 
               (Allocate name1 (es1 * n) t1
               (Allocate name2 (es2 * n) t2
                p))) Skip
    return (zipp (result1,result2))

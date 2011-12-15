{-# LANGUAGE FlexibleInstances, 
             FlexibleContexts, 
             MultiParamTypeClasses,  
             UndecidableInstances, 
             TypeFamilies,  
             GADTs  #-}  
module Obsidian.GCDObsidian.Sync where 

import Obsidian.GCDObsidian.Kernel
import Obsidian.GCDObsidian.Exp
import Obsidian.GCDObsidian.Array
import Obsidian.GCDObsidian.Types
import Obsidian.GCDObsidian.Program
import Obsidian.GCDObsidian.Library



import Control.Monad.Writer
import Data.Word

----------------------------------------------------------------------------
-- Library functions that use Sync  


composeS [] = pure id
composeS (f:fs) = f ->- sync ->- composeS fs




----------------------------------------------------------------------------
-- Sync

pSyncArray  :: Scalar a => Array Pull (Exp a) -> Kernel (Array Pull (Exp a))
pSyncArray arr = 
  do 
    name <- newArray
    
    let p = parr !* (targetArray name)
         
    tell$ 
        (Allocate name (es * (len arr)) t ()) 
        `ProgramSeq`
        p 
        `ProgramSeq`
        (Synchronize True)
            
    return$ mkPullArray (index name) (len arr)
      
  where 
    es = fromIntegral$ sizeOf (arr ! 0) 
    t  = Pointer$ Local$ typeOf (arr ! 0)

    parr = push arr 
        

pSyncArrayP :: Scalar a => Array Push (Exp a) -> Kernel (Array Pull (Exp a)) 
pSyncArrayP arr  = 
  do 
    name <- newArray
    
    let n = len arr
        result = mkPullArray (index name) n         
        es = fromIntegral$ sizeOf (result ! 0) 
        t  = Pointer$ Local$ typeOf (result ! 0)
        p  = arr !* (targetArray name)

    tell$ (Allocate name (es * n) t () )
          `ProgramSeq`
          p  
          `ProgramSeq`
          (Synchronize True)
          
    return result

pSyncArrayP2 :: (Scalar a, Scalar b ) => Array Push (Exp a,Exp b) -> Kernel (Array Pull (Exp a,Exp b))
pSyncArrayP2 arr =  
  do 
    name1 <- newArray
    name2 <- newArray
                      
    let n = len arr
        result1 = mkPullArray (index name1) n
        result2 = mkPullArray (index name2) n
        t1 = Pointer$ Local$ typeOf (result1 ! 0) 
        t2 = Pointer$ Local$ typeOf (result2 ! 0)
        es1 = fromIntegral$ sizeOf (result1 ! 0)
        es2 = fromIntegral$ sizeOf (result2 ! 0)
        p  = arr !* (targetPair name1 name2)
        
    tell$ (Allocate name1 (es1 * n) t1 ()) *>* 
          (Allocate name2 (es2 * n) t2 ()) *>* 
          p
          *>* 
          (Synchronize True)
    return (zipp (result1,result2))

-- TODO: is this an approach to more general syncs ? (see limitations on Syncable class) 
class Syncable' a where 
  type Synced a  
  sync' :: a -> Kernel (Synced a) 
  
instance Syncable (Array Pull) (Exp a) => Syncable' (Array Pull (Exp a)) where 
  type Synced (Array Pull (Exp a)) = Array Pull (Exp a) 
  sync' = sync 
  
instance Syncable (Array Push)  (Exp a)  => Syncable' (Array Push (Exp a)) where 
  type Synced (Array Push (Exp a)) = Array Pull (Exp a) 
  sync' = sync 


instance (Syncable' a, Syncable' b) => Syncable' (a,b) where 
  type Synced (a,b) = (Synced a, Synced b) 
  sync' (a1,a2) = 
    do  
      a1' <- sync' a1
      a2' <- sync' a2
      return (a1',a2') 

         
   
  
-- TODO: Here only possible to sync on arrays ? 
class Syncable a b where 
  sync :: a b -> Kernel (Array Pull b)
 
instance (Scalar a) => Syncable (Array Pull) (Exp a) where 
  sync = pSyncArray

instance (Syncable (Array Pull) a, Syncable (Array Pull) b) => Syncable (Array Pull) (a,b) where
  sync arr = do
    a1' <- sync a1
    a2' <- sync a2
    return$ zipp (a1',a2')
    where 
      (a1,a2) = unzipp arr

instance (Syncable (Array Pull) a, Syncable (Array Pull) b, Syncable (Array Pull) c) 
         => Syncable (Array Pull) (a,b,c) where
  sync arr = do 
    a1' <- sync a1 
    a2' <- sync a2 
    a3' <- sync a3
    return$ zipp3 (a1',a2',a3')
    where 
      (a1,a2,a3) = unzipp3 arr
 
instance Scalar a => Syncable (Array Push) (Exp a) where  
  sync arr =  pSyncArrayP arr
  
-- GAH! not good !! 
instance (Scalar a, Scalar b) => Syncable (Array Push) (Exp a, Exp b) where 
  sync =  pSyncArrayP2 

              
    

{- 
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
-}
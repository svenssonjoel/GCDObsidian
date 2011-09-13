{-# LANGUAGE FlexibleContexts #-}

module Obsidian.GCDObsidian.Store  
       (sync,
        sync2, 
        store,
        storeP,
        storeIlv,
--        storeIlvF,
--        storeCatZ
        )  where 

import Obsidian.GCDObsidian.Exp 
import Obsidian.GCDObsidian.Array
import Obsidian.GCDObsidian.Kernel
import Obsidian.GCDObsidian.Elem
import Obsidian.GCDObsidian.Library


import Control.Monad.State
import Control.Monad.Writer

import Data.Word


------------------------------------------------------------------------------
-- TODO: Start looking at what porblems the "dynamic" arrays might cause


------------------------------------------------------------------------------
-- Store
store :: (OArray a e, Scalar e) => a e -> Kernel (a e)
store = sync
        
storeP :: (OArray a e, Scalar e, 
           OArray a' e', Scalar e') 
          => (a e,a' e') 
          -> Kernel (a e,a' e') 
storeP = sync2

sync :: (OArray a e, Scalar e) => a e -> Kernel (a e)
sync arr = 
  do 
    let ll@(LLArray ixf n m d) = toLL arr 
    
    (s,r) <- singleSyncUnit ll -- writeNT ll 
    tell$ code$ s -- Store m w 
    return$ fromLL r
    
singleSyncUnit ll@(LLArray ixf n m d) = 
  do 
    let elmsize = fromIntegral$ sizeOf (ixf (variable "X"))
    newName <- newArray (elmsize * m) 
    let newArray = LLArray (\ix -> Index (newName,[ix])) n m d 
                
    return (SyncUnit m 
               (StoreListCons 
                  (Store newName 
                         (m*elmsize) 
                         [write newName ll ()]) StoreListNil),newArray)
                    
-- A number of writes into a single named C-Style array      
mkStore :: Name -> Word32 -> [Write a extra] -> Store a extra   
mkStore name bytes ws = Store name bytes ws 

mkWrite :: Scalar a => (Exp Word32 -> Exp Word32) -> LLArray a -> extra -> Write a extra
mkWrite targf ll e = Write targf ll e 

singleStore ll@(LLArray ixf n m d) = 
  do
    let elmsize = fromIntegral$ sizeOf (ixf (variable "X"))
    newName <- newArray (elmsize * m)  -- TODO: no longer need the ArraysSizes map
    let newArray = LLArray (\ix -> Index (newName,[ix])) n m d 
    
    let ws = mkWrite id ll ()
    
    return (mkStore newName (m*elmsize) [ws],newArray) 
  
sync2 :: (OArray a e, Scalar e, 
          OArray a' e', Scalar e') 
         => (a e,a' e') 
         -> Kernel (a e,a' e') 
sync2 (a1,a2)  = 
  do         
    (w1,r1) <- singleStore ll1 
    (w2,r2) <- singleStore ll2
    
    tell$ code$ SyncUnit numThreads (StoreListCons w1  
                                      (StoreListCons w2 StoreListNil))
    
    return$ (fromLL r1,fromLL r2)  
    where 
      ll1 = toLL a1 
      ll2 = toLL a2   
      numThreads = gcd (staticLength ll1) (staticLength ll2)

             
             
------------------------------------------------------------------------------
--
storeIlv ::( Scalar e) 
             => (Array (e,e)) 
             -> Kernel (Array (e,e)) 
storeIlv arr = do 
  (w,(r1,r2)) <- writeIlv ll1 ll2
  
  tell$ code$ w
  
  return$ zipp (fromLL r1,fromLL r2)
  
  where 
    (a1,a2) = unzipp arr
    ll1 = toLL a1 
    ll2 = toLL a2
   


writeIlv ll1@(LLArray ixf n m d) 
         ll2@(LLArray ixf' n' m' d') = 
  do 
    --i <- get 
    --put (i+1) 
    --let newName  = "arr" ++ show i
    let elmsize1 = fromIntegral$ sizeOf$ ixf (variable "X")
    newName <- newArray (elmsize1 * (m+m'))
    let newArray1 = LLArray (\ix -> Index (newName,[ix*2])) n m d 
        newArray2 = LLArray (\ix -> Index (newName,[ix*2+1])) n' m' d' 
        numThreads = staticLength ll1
        
    return (SyncUnit numThreads 
                     (StoreListCons
                      (Store newName 
                             (2*elmsize1*numThreads) 
                             [Write (*2) ll1 (),
                             Write (\ix -> ix*2+1) ll2 ()])
                      StoreListNil),(newArray1,newArray2))

{- 
storeIlvF ::( Scalar e) 
             => (Array (e,e)) 
             -> Kernel (Array e) 
storeIlvF arr = do 
  (w,r) <- writeIlvF ll1 ll2
  
  tell$ code$ Store numThreads w
  
  return$ fromLL r
  
  where 
    (a1,a2) = unzipp arr
    ll1 = toLL a1 
    ll2 = toLL a2
    numThreads = staticLength ll1


writeIlvF ll1@(LLArray ixf n m d) 
          ll2@(LLArray ixf' n' m' d') = 
  do 
    --i <- get 
    --put (i+1) 
    
    --let newName  = "arr" ++ show i
    let elmsize1 = fromIntegral$ sizeOf (ixf (variable "X"))
    newName <- newArray (elmsize1 * (m+m'))
    let newArray = LLArray (\ix -> Index (newName,[ix])) (n+n') (m+m') d 
        
    return ([Write (\ix -> index newName (ix*2)) ll1 (),
             Write (\ix -> index newName (ix*2+1)) ll2 ()] ,newArray)


storeCatZ ::( Scalar e) 
             => (Array (e,e)) 
             -> Kernel (Array e) 
storeCatZ arr = do 
  (w,r) <- writeCatZ ll1 ll2
  
  tell$ code$ Store numThreads w
  
  return$ fromLL r
  
  where 
    (a1,a2) = unzipp arr
    ll1 = toLL a1 
    ll2 = toLL a2
    numThreads = staticLength ll1


writeCatZ ll1@(LLArray ixf n m d) 
          ll2@(LLArray ixf' n' m' d') = 
  do 
    --i <- get 
    --put (i+1) 
    --let newName  = "arr" ++ show i
    let elmsize1 = fromIntegral$ sizeOf (ixf (variable "X"))
    newName <- newArray (elmsize1 * (m+m'))               
                   
    let newArray = LLArray (\ix -> Index (newName,[ix])) (n+n') (m+m') d 
        -- newArray2 = LLArray (\ix -> Index (newName,[ix*2+1])) n' m' d' 
    return ([Write (\ix -> index newName (ix)) ll1 (),
             Write (\ix -> index newName (ix+(fromIntegral m))) ll2 ()] ,newArray)



------------------------------------------------------------------------------
--
  
------------------------------------------------------------------------------  
-- Combination of store and two 
  
twoK' :: Int -> (Array a -> Array b) -> Array a -> Kernel (Array b) 
twoK' = undefined 

-}
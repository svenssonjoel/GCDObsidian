{-# LANGUAGE FlexibleContexts #-}

module Obsidian.GCDObsidian.Store  
      {- (sync,
        sync2, 
        store,
        storeP
        ) -} where 

import Obsidian.GCDObsidian.Exp 
import Obsidian.GCDObsidian.Array
import Obsidian.GCDObsidian.Kernel
import Obsidian.GCDObsidian.Elem

import Control.Monad.State
import Control.Monad.Writer

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
    
    (w,r) <- writeNT ll 
    tell$ code$ Store m w 
    return$ fromLL r
    
  
sync2 :: (OArray a e, Scalar e, 
          OArray a' e', Scalar e') 
         => (a e,a' e') 
         -> Kernel (a e,a' e') 
sync2 (a1,a2)  = 
  do         
    (w1,r1) <- writeNT ll1
    (w2,r2) <- writeNT ll2
    
    tell$ code$ Store numThreads (w1++w2)
    
    return$ (fromLL r1,fromLL r2)  
    where 
      ll1 = toLL a1 
      ll2 = toLL a2   
      numThreads = gcd (staticLength ll1) (staticLength ll2)

------------------------------------------------------------------------------
--move to library
unzipp :: (Elem a , Elem b) =>  Array (a,b) -> (Array a, Array b)       
unzipp arr = (Array (\ix -> fst (untup2 (arr ! ix))) (len arr),
              Array (\ix -> snd (untup2 (arr ! ix))) (len arr))
              
zipp :: (Elem a, Elem b) => (Array a, Array b) -> Array (a,b)             
zipp (arr1,arr2) = Array (\ix -> tup2 (arr1 ! ix, arr2 ! ix)) (len arr1)
             
             
------------------------------------------------------------------------------
--

storeIlv ::( Scalar e) 
             => (Array (e,e)) 
             -> Kernel (Array (e,e)) 
storeIlv arr = do 
  (w,(r1,r2)) <- writeIlv ll1 ll2
  
  tell$ code$ Store numThreads w
  
  return$ zipp (fromLL r1,fromLL r2)
  
  where 
    (a1,a2) = unzipp arr
    ll1 = toLL a1 
    ll2 = toLL a2
    numThreads = staticLength ll1


writeIlv ll1@(LLArray ixf n m d) 
         ll2@(LLArray ixf' n' m' d') = 
  do 
    i <- get 
    put (i+1) 
    let newName  = "arr" ++ show i
        elmsize1 = sizeOf (ixf (variable "X"))
        newArray1 = LLArray (\ix -> Index (newName,[ix*2])) n m d 
        newArray2 = LLArray (\ix -> Index (newName,[ix*2+1])) n' m' d' 
    return ([Write (\ix -> index newName (ix*2)) ll1 (),
             Write (\ix -> index newName (ix*2+1)) ll2 ()],(newArray1,newArray2))


------------------------------------------------------------------------------
--
writeNT ll@(LLArray ixf n m d) = 
  do 
    i <- get 
    put (i+1) 
    let newName  = "arr" ++ show i
        newArray = LLArray (\ix -> Index (newName,[ix])) n m d 
                
    return ([write newName ll () ],newArray)
                    
  
------------------------------------------------------------------------------  
--

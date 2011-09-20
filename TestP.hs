
module TestP where 

import Obsidian.GCDObsidian.Kernel 
import Obsidian.GCDObsidian.Array 
import Obsidian.GCDObsidian.Library 
import Obsidian.GCDObsidian.Exp 

  
small1 :: Array (Data Int) -> Array (Data Int) -> PKernel (Array (Data Int))
small1 arr1 arr2 = pSyncArrayP part
  where
    part = concP arr1' arr2'  
    arr1' = toArrayP arr1
    arr2' = toArrayP arr2 
  

small2 :: Array (Data Int) -> Array (Data Int) -> PKernel (Array (Data Int))
small2 arr1 arr2 = pSyncArray part
  where
    part = conc (arr1,arr2)  

    
small3 :: Array (Data Int) -> PKernel (Array (Data Int)) 
small3 arr = pSyncArrayP b 
  where 
    a = rev arr 
    b = revP (toArrayP a) 
    
    
small4 :: Array (Data Int) -> Array (Data Int) -> PKernel (Array (Data Int),Array (Data Int)) 
small4 a1 a2 = pSyncArrays (a1,a2) 


small5 :: Array (Data Int) -> PKernel (Array (Data Int)) 
small5 arr = 
  do 
    a0 <- pSyncArray arr
    a1 <- pSyncArray a0
    a2 <- pSyncArray a1
    a3 <- pSyncArray a2
    a4 <- pSyncArray a0
    return a4
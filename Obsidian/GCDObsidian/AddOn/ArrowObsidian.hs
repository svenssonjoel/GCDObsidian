{-# LANGUAGE TypeOperators, 
             GADTs, 
             FlexibleContexts #-} 
module Obsidian.GCDObsidian.AddOn.ArrowObsidian where 


import Obsidian.GCDObsidian.Exp 
import Obsidian.GCDObsidian.Kernel
import Obsidian.GCDObsidian.Program
import Obsidian.GCDObsidian.Array
import Obsidian.GCDObsidian.Sync


{- an attempt at implementing old ArrowObsidian as a 
   library on top of new "MonadObsidian" (GCDObsidian) -} 

data a :-> b where 
  Pure :: (a -> b) -> (a :-> b) 
  Sync :: Syncable Array b => (a -> Array b) -> (Array b -> c) -> 
          (a :-> c) 
          
          
          
runArrow :: (a :-> b) -> a -> Kernel b 
runArrow (Pure f) a   = return$ f a
runArrow (Sync f g) a = 
  do 
    a' <- sync (f a) 
    return$ g a'
{-# LANGUAGE MultiParamTypeClasses,
             FlexibleInstances,
             ScopedTypeVariables #-} 

module Obsidian.GCDObsidian.Force where


import Obsidian.GCDObsidian.Program
import Obsidian.GCDObsidian.Exp
import Obsidian.GCDObsidian.Array
import Obsidian.GCDObsidian.Types

---------------------------------------------------------------------------
-- Class of Forceables
--------------------------------------------------------------------------- 

class Forceable a p e where
  force :: a p e -> Program (a Pull e) 


---------------------------------------------------------------------------
-- Base cases
---------------------------------------------------------------------------
  
instance Scalar a => Forceable Array Pull (Exp a) where
  force arr =
    do 
    name <- Allocate n $ Pointer (typeOf (undefined :: (Exp a)))
    p (targetArr name)
    Sync
    return $ Array n $ Pull (\i -> index name i)
    where
      (Array n (Push p)) = push arr 
      targetArr name (i,e) = Assign name i e

instance Scalar a => Forceable Array Push (Exp a) where
  force (Array n (Push p)) =
    do 
    name <- Allocate n $ Pointer (typeOf (undefined :: (Exp a)))
    p (targetArr name)
    Sync
    return $ Array n $ Pull (\i -> index name i)
    where
      targetArr name (i,e) = Assign name i e


---------------------------------------------------------------------------
-- Also deal with pairs etc.. (future work)
---------------------------------------------------------------------------
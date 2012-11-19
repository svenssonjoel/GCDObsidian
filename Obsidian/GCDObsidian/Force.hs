{- Joel Svensson 2012 -}
{-# LANGUAGE MultiParamTypeClasses,
             FlexibleInstances,
             ScopedTypeVariables,
             TypeFamilies #-} 

module Obsidian.GCDObsidian.Force where


import Obsidian.GCDObsidian.Program
import Obsidian.GCDObsidian.Exp
import Obsidian.GCDObsidian.Array
import Obsidian.GCDObsidian.Blocks
import Obsidian.GCDObsidian.Types

{- 
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

-} 

---------------------------------------------------------------------------
-- Also deal with pairs etc.. (future work)
---------------------------------------------------------------------------


---------------------------------------------------------------------------
-- New Approach to Forceable. 
---------------------------------------------------------------------------
class Forceable a where
  type Forced a 
  force :: a -> Forced a


---------------------------------------------------------------------------
-- Force local
---------------------------------------------------------------------------
instance Scalar a => Forceable (Array Pull (Exp a)) where
  type Forced (Array Pull (Exp a)) = Program (Array Pull (Exp a))
  force arr = force (push arr) 

instance Scalar a => Forceable (Array Push (Exp a)) where
  type Forced (Array Push (Exp a)) = Program (Array Pull (Exp a)) 
  force (Array n (Push p)) =
    do
      -- Allocate is a bit strange since
      -- it wants the n in bytes! But also knows the type. 
    name <- Allocate (n * fromIntegral (sizeOf (undefined :: Exp a)))
                     (Pointer (typeOf (undefined :: (Exp a))))
    p (targetArr name)
    Sync
    return $ Array n $ Pull (\i -> index name i)
    where
      targetArr name (i,e) = Assign name i e

---------------------------------------------------------------------------
-- Force Global Pull Array 
---------------------------------------------------------------------------

instance Scalar a => Forceable (Blocks (Array Pull (Exp a))) where 
  type Forced (Blocks (Array Pull (Exp a))) = 
    Program (Blocks (Array Pull (Exp a)))
  force bs = force (fmap push bs)

---------------------------------------------------------------------------
-- Force Global Push Array 
---------------------------------------------------------------------------
instance Scalar a => Forceable (Blocks (Array Push (Exp a))) where 
  type Forced (Blocks (Array Push (Exp a))) = 
    Program (Blocks (Array Pull (Exp a)))

  force (Blocks n bxf) =  
    do
      global <- Output $ Pointer (typeOf (undefined :: (Exp a))) 

      -- dryrun to get length. 
      let (Array s (Push pfun)) =  bxf (variable "dummy") 
    
      ForAllBlocks n
        (\bid ->
          do
            let (Array s (Push pfun)) = bxf bid 
            pfun (assignTo global (bid, s)))
     
      return $ Blocks n  $ 
        \bix -> Array s (Pull (\ix -> index global ((bix * (fromIntegral s)) + ix)))
    where 
      assignTo name (bid,s) (i,e) = Assign name ((bid*(fromIntegral s))+i) e       

---------------------------------------------------------------------------
-- Force Global Program of Pull Array 
---------------------------------------------------------------------------
instance Scalar a =>
         Forceable (Blocks (Program (Array Pull (Exp a)))) where
  type Forced (Blocks (Program (Array Pull (Exp a)))) =
    Program (Blocks (Array Pull (Exp a)))

  force (Blocks n bxf) =  
    do
      global <- Output $ Pointer (typeOf (undefined :: (Exp a))) 

      -- dryrun to get length. 
      (Array s (Pull pfun)) <- bxf (variable "dummy") 
    
      ForAllBlocks n
        (\bid ->
          do
            arr <- bxf bid
            let (Array s (Push pfun)) = push arr 
            pfun (assignTo global (bid, s)))
     
      return $ Blocks n  $ 
        \bix -> Array s (Pull (\ix -> index global ((bix * (fromIntegral s)) + ix)))
    where 
      assignTo name (bid,s) (i,e) = Assign name ((bid*(fromIntegral s))+i) e 
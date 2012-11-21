{-# LANGUAGE MultiParamTypeClasses,  
             FlexibleInstances  #-} 

module Obsidian.GCDObsidian.Array  where

import Obsidian.GCDObsidian.Exp 
import Obsidian.GCDObsidian.Types
import Obsidian.GCDObsidian.Globs
import Obsidian.GCDObsidian.Program


import Data.List
import Data.Word


---------------------------------------------------------------------------
-- Push and Pull arrays
---------------------------------------------------------------------------
type P a = (a -> Program ()) -> Program ()


data Push a = Push {pushFun :: P (Exp Word32,a)}

data PushP r a = PushP ((a -> r) -> r) 


--                             nBlocks     elt/block
data PushGlob a = PushGlobal (Exp Word32) Word32
                             ((a -> Exp Word32 -> Exp Word32 -> Program ())
                              -> Program ())
-- Global Computations
type PushBT = PushP (Exp Word32 -> Exp Word32 -> Program ())
-- Local Computations
type PushT  = PushP (Exp Word32 -> Program ())

--data PushP r a = PushP ((a -> r) -> r) 
{-
  PushP:
   Think about what the r parameter looks like.
   So. if we add a thread id here we can remove the "ForAll"
   constructor from the Program type.

   In the PushBT case a BlockId -> Program () is the result
   this could also be avoided by having a ForAllBlocks in
   the program () type. 

-}  
--type PushBT = PushP (Exp Word32 -> Exp Word32 -> Program ())
--type PushT  = PushP (Exp Word32 -> Program ())

data Pull a = Pull {pullFun :: Exp Word32 -> a}

mkPush :: (((Exp Word32, a) -> Program ())
           -> Program ()) -> Push a
mkPush p = Push p  

data Array p a = Array Word32 (p a) 
data GlobArray p a = GlobArray (Exp Word32) Word32 (p a)

type PushArray a = Array Push a 
type PullArray a = Array Pull a 

mkPushArray :: Word32 -> (((Exp Word32, a) -> Program ())
                         -> Program ()) -> PushArray a
mkPushArray n p = Array n (Push p) 
mkPullArray n p = Array n (Pull p)  

resize m (Array n p) = Array m p 


{- 
-- TODO: Do you need (Exp e) where there is only e ? 
class  PushyInternal a where 
  push' :: Word32 -> a e -> Array Push e  
  push'' :: Word32 -> a e -> Array Push e 



  
instance PushyInternal (Array Pull)  where   
  push' m (Array n (Pull ixf)) = 
    Array n $ mkPush $ \k ->
                      ForAll (n `div` m)
                      (\i -> foldr1 (*>*) 
                             [k (ix,a)
                             | j <-  [0..m-1],
                               let ix = (i*(fromIntegral m) + (fromIntegral j)),
                               let a  = ixf ix
                             ]) 
  push'' m (Array n (Pull ixf)) = 
    Array n $mkPush $ \k ->
                      ForAll (n `div` m)
                      (\i -> foldr1 (*>*) 
                             [k (ix,a)
                             | j <-  [0..m-1],
                               let ix = (i+((fromIntegral ((n `div` m) * j)))),
                               let a  = ixf ix
                             ]) 
-} 
class Pushable a where 
  push :: a e -> Array Push e 

instance Pushable (Array Push) where 
  push = id 
  
instance Pushable (Array Pull)  where   
  push (Array n (Pull ixf)) =
    Array n $
    mkPush $ \k -> ForAll n (\i -> k (i,(ixf i)))

---------------------------------------------------------------------------
--
---------------------------------------------------------------------------
namedArray name n = mkPullArray n (\ix -> index name ix)
indexArray n      = mkPullArray n (\ix -> ix)

class Indexible a e where 
  access :: a e -> Exp Word32 -> e 
  
instance Indexible (Array Pull) a where
  access (Array _ ixf) ix = pullFun ixf ix


pushApp (Array n (Push p)) a = p a 

{- 
class PushApp a where 
  papp :: a e -> ((Exp Word32,e) -> Program ()) -> Program ()

instance PushApp (Array Push) where 
  papp (Array _ (Push (P f))) a = f a 

instance PushApp (GlobalArray Push) where 
  papp (GlobalArray (Push f) n) a = f a 
  -} 

class Len a where 
  len :: a e -> Word32

instance Len (Array p) where 
  len (Array n _) = n 

infixl 9 ! 
(!) :: Indexible a e => a e -> Exp Word32 -> e 
(!) = access
{- 
 
-- infixl 9 !* 
-- (!*) :: PushApp a => a e -> ((Exp Word32,e) -> Program ()) -> Program ()
-- (!*) :: PushApp a => a e -> 
-- (!*) p a = papp p a 
         
-} 


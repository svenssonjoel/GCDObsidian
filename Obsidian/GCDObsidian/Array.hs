{-# LANGUAGE MultiParamTypeClasses, 
             FlexibleInstances,
             FlexibleContexts, 
             UndecidableInstances,  
             GADTs #-} 

module Obsidian.GCDObsidian.Array
{- 
       ((!) -- pull array apply (index into)
                                  ,(!*) -- push array apply 
                                  , mkPullArray
                                  , mkPushArray
                                  , resize
                                  , namedArray
                                  , indexArray
                                  , len 
                                  , globLen
                                  , Array(..)  
                                  , Pushy
                                  , PushyInternal
                                  , pushGlobal
                                  , push
                                  , push' -- this is for "internal" use
                                  , push'' -- this is for "internal" use
                                  , P(..)
                                  , block
                                  , unblock
                                  , GlobalArray(..)
                                  , mkGlobalPushArray  
                                  , mkGlobalPullArray
                                  , Pull(..)
                                  , Push(..)
                                  )
-}where 

import Obsidian.GCDObsidian.Exp 
import Obsidian.GCDObsidian.Types
import Obsidian.GCDObsidian.Globs
import Obsidian.GCDObsidian.Program


import Data.List
import Data.Word


----------------------------------------------------------------------------
-- 


------------------------------------------------------------------------------
data Push a = Push {pushFun :: P (Exp Word32,a)}
data Pull a = Pull {pullFun :: Exp Word32 -> a}

--What about
data PushG a = PushG {pushGFun :: P (Exp Word32,Exp Word32, a)}
data PullG a = PullG {pullGFun :: Exp Word32 -> Exp Word32 -> a} 



{- 
   data Push ix a = Push {pushFun :: P (ix,a))
   data Pull ix a = Pull {pullFun :: ix -> a)) 

   data Dim1 = Dim1  Word32 
   data Dim2 = Dim2  Word32 Word32  
   data Dim3 = Dim3  Word32 Word32 Word32
 
   data Array p a d = Array (p a) d


   type PullArray   a = Array (Pull Ix1D a) Dim1 
   type PullArray2D a = Array (Pull Ix2D a) Dim2  
   type PullArray3D a = Array (Pull Ix3D a) Dim3
  
   type PushArray   a = Array (Push Ix1D a) Dim1 
   type PushArray2D a = Array (Push Ix2D a) Dim2
   type PushArray3D a = Array (Push Ix3D a) Dim3 
   

   What happens once someone tries to nest these.. 
   PullArray3D (PullArray3D (Exp Int)) 

   More things to consider here:  
     - Grid dimensions will be FIXED throughout the execution 
       of a kernel. 
     - Maybe it is better to Emulate the 2d and 3d blocks. 
       For example a single kernel might handle an array of size 256 
       and a at the same time a 16*16 Array2D. This means this kernel 
       needs to use 256 threads. But does it need 256 threads as 16*16 or 256*1.
       Of course only one option is possible and either way leads to some extra arith.
         (arr256[tid.y*16+tid.x] and arr16x16[tid.y][tid.x]) or 
         (arr256[tid.x] and arr16x16[tid.x `div` 16][tid.x `mod` 16]
    - This can get more complicated.  
      A single kernel could operate on many different multidimensional arrays. 
      arr16x16 and arr4x12 for example. This would lead to things like 
      if (threadIdx.x < 12 && threadIdx.y < 4 ) { 
         arr4x12[threadIdx.y][threadIdx.x] = ...
      } 
      arr16x16[threadIdx.y][threadIdx.x] = ... 
    - And even worse!!
      arr16x16 and arr128x4 
      
    - Add 3D arrays to this mix and it gets very complicated.   
    


-} 

newtype P a = P {unP :: (a -> Program ()) -> Program ()}  

data Array p a = Array (p a) Word32

type PushArray a = Array Push a 
type PullArray a = Array Pull a

mkPushArray p n = Array (Push (P p)) n 
mkPullArray p n = Array (Pull p) n 

resize (Array p n) m = Array p m 


runP :: P a -> (a -> Program ()) -> Program ()
runP p a = (unP p) a  

instance Monad P where 
  return a = P $ \k -> k a 
  (>>=) (P m) f = P $ \k -> m (\a -> runP  (f a) k) 

instance Functor P where 
  fmap f (P m) = P $ \k -> m (\a -> k (f a))  


    
--instance Applicative P where 
--  ...
 


-- TODO: Do you need (Exp e) where there is only e ? 
class  PushyInternal a where 
  push' :: Word32 -> a e -> Array Push e  
  push'' :: Word32 -> a e -> Array Push e 

instance PushyInternal (Array Pull)  where   
  push' m (Array (Pull ixf) n) = 
    Array (Push (P (\k ->
                  ForAll (n `div` m)
                         (\i -> foldr1 (*>*) 
                                [k (ix,a)
                                | j <-  [0..m-1],
                                  let ix = (i*(fromIntegral m) + (fromIntegral j)),
                                  let a  = ixf ix
                                ])))) n
  push'' m (Array (Pull ixf) n) = 
    Array (Push ( P (\k ->
                  ForAll (n `div` m)
                         (\i -> foldr1 (*>*) 
                                [k (ix,a)
                                | j <-  [0..m-1],
                                  let ix = (i+((fromIntegral ((n `div` m) * j)))),
                                  let a  = ixf ix
                                ])))) n

class Pushy a p e where
  push :: a p e -> a Push e

instance Pushy Array Push e where 
  push = id 
  
instance Pushy Array Pull e  where   
  push (Array (Pull ixf) n) =
    Array (Push (P (\k ->
                  ForAll n (\i -> k (i,ixf i))))) n 

  
{-     
class Pushy a where 
  push :: a e -> Array Push e 

instance Pushy (Array Push) where 
  push = id 
  
instance Pushy (Array Pull)  where   
  push (Array (Pull ixf) n) = Array (Push (\func -> ForAll (\i -> func (i,ixf i)) n)) n 
-}
{- 
class PushGlobal a where 
  pushGlobal :: a e -> GlobalArray Push e 

instance PushGlobal (GlobalArray Pull) where 
  pushGlobal (GlobalArray (Pull ixf) n) = 
      GlobalArray (Push (\func -> ForAllGlobal (\i -> func (i,ixf i)) n )) n
-}
----------------------------------------------------------------------------
--

namedArray name n = mkPullArray (\ix -> index name ix) n 
indexArray n      = mkPullArray (\ix -> ix) n 

class Indexible a e where 
  access :: a e -> Exp Word32 -> e 
  
instance Indexible (Array Pull) a where
  access (Array ixf _) ix = pullFun ixf ix

class PushApp a where 
  papp :: a e -> ((Exp Word32,e) -> Program ()) -> Program ()

instance PushApp (Array Push) where 
  papp (Array (Push f) n) a = (unP f) a 

--instance PushApp (GlobalArray Push) where 
--  papp (GlobalArray (Push f) n) a = f a 
  
class Len a where 
  len :: a e -> Word32

instance Len (Array p) where 
  len (Array _ n) = n 
  
infixl 9 ! 
(!) :: Indexible a e => a e -> Exp Word32 -> e 
(!) = access

infixl 9 !* 
(!*) :: PushApp a => a e -> ((Exp Word32,e) -> Program ()) -> Program () 
(!*) p a = papp p a 

------------------------------------------------------------------------------
-- Show 

instance Show  a => Show (Array Pull a) where
  show arr | len arr <= 10 =  "[" ++ 
                              (concat . intersperse ",") 
                              [show (arr ! (fromIntegral i)) | i <- [0..len arr-1]] ++ 
                              "]"
           | otherwise     =  "[" ++ 
                              (concat . intersperse ",") 
                              [show (arr ! (fromIntegral i)) | i <- [0..3]] ++ 
                              "...]"

         

--------------------------------------------------------------------------
-- Global array related stuff
-- This is also quite directly influencing "coordination" 
-- of kernels. 
{- 
data GlobalArray p a = GlobalArray (p a) (Exp Word32)

mkGlobalPushArray p n = GlobalArray (Push p) n 
mkGlobalPullArray f n = GlobalArray (Pull f) n 

instance Functor (GlobalArray Pull) where 
  fmap f (GlobalArray (Pull g) n) = GlobalArray (Pull (f . g)) n 

instance Indexible (GlobalArray Pull) a where  
  access (GlobalArray ixf _) ix = pullFun ixf ix
  
globLen (GlobalArray _ n) = n

-} 

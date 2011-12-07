{-# LANGUAGE MultiParamTypeClasses, 
             FlexibleInstances,
             FlexibleContexts, 
             UndecidableInstances  #-} 

module Obsidian.GCDObsidian.Array ((!) -- pull array apply (index into)
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
                                 -- , pushApp
                                  , push
                                  , push' -- this is for "internal" use
                                  , push'' -- this is for "internal" use
                                 -- , ArrayP(..)
                                  , P(..)
                                  , block
                                  , unblock
                                  , GlobalArray(..)
                                  , mkGlobalPushArray  
                                  , mkGlobalPullArray
                                  , Pull(..)
                                  , Push(..)
                                  -- , pushGlobal
                                  )where 

import Obsidian.GCDObsidian.Exp 
import Obsidian.GCDObsidian.Types
import Obsidian.GCDObsidian.Globs
import Obsidian.GCDObsidian.Program


import Data.List
import Data.Word

------------------------------------------------------------------------------

data Push a = Push {pushFun :: P (Exp Word32,a)}
data Pull a = Pull {pullFun :: Exp Word32 -> a}


-- Arrays!
--data Array a = Array (Exp Word32 -> a) Word32 
--data Array a = Array (Exp Word32 -> a)  Word32 
-- PUSHY ARRAYS! 

type P a = (a -> Program ()) -> Program () 

data Array p a = Array (p a) Word32

type PushArray a = Array Push a 
type PullArray a = Array Pull a 

mkPushArray p n = Array (Push p) n 
mkPullArray p n = Array (Pull p) n 

resize (Array p n) m = Array p m 


{- 
To look at later !!!! (needs to be a newtype though!
instance Monad P where 
  return a = P $ \k -> k a 
  (>>=) (P m) f = P $ \k -> m (\a -> runP (f a) k) 

instance Functor P where 
  ... 

instance Applicative P where 
  ...

-} 

-- data ArrayP a = ArrayP (P (Exp Word32, a)) Word32

-- data ArrayP a = ArrayP ((Exp Word32 -> a -> Program ()) -> Program ()) Word32

-- pushApp (ArrayP func n) a =  func a 


-- TODO: Do you need (Exp e) where there is only e ? 
class  PushyInternal a where 
  push' :: Word32 -> a e -> Array Push e  
  push'' :: Word32 -> a e -> Array Push e 

instance PushyInternal (Array Pull)  where   
  push' m (Array (Pull ixf) n) = 
    Array (Push (\func -> ForAll (\i -> foldr1 (*>*) 
                                   [func (ix,a)
                                   | j <-  [0..m-1],
                                     let ix = (i*(fromIntegral m) + (fromIntegral j)),
                                     let a  = ixf ix
                                   ]) (n `div` m))) n
  push'' m (Array (Pull ixf) n) = 
    Array (Push (\func -> ForAll (\i -> foldr1 (*>*) 
                                   [func (ix,a)
                                   | j <-  [0..m-1],
                                     let ix = (i+((fromIntegral ((n `div` m) * j)))),
                                     let a  = ixf ix
                                   ]) (n `div` m))) n
    

         
class Pushy a where 
  push :: a e -> Array Push e 

instance Pushy (Array Push) where 
  push = id 
  
instance Pushy (Array Pull)  where   
  push (Array (Pull ixf) n) = Array (Push (\func -> ForAll (\i -> func (i,(ixf i))) n)) n 

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
  papp (Array (Push f) n) a = f a 

instance PushApp (GlobalArray Push) where 
  papp (GlobalArray (Push f) n) a = f a 
  
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

data GlobalArray p a = GlobalArray (p a) (Exp Word32)

mkGlobalPushArray p n = GlobalArray (Push p) n 
mkGlobalPullArray f n = GlobalArray (Pull f) n 

instance Functor (GlobalArray Pull) where 
  fmap f (GlobalArray (Pull g) n) = GlobalArray (Pull (f . g)) n 

instance Indexible (GlobalArray Pull) a where  
  access (GlobalArray ixf _) ix = pullFun ixf ix
  
globLen (GlobalArray _ n) = n



 -- TODO: These should be somewhere else !!! 
block :: Word32 -> GlobalArray Pull a -> Array Pull a   
block blockSize glob = Array (Pull newFun) blockSize 
  where 
    newFun ix = (pullFun pull) ((bid * (fromIntegral blockSize)) + ix)  
    (GlobalArray pull n) = glob 

bid   = BlockIdx X -- variable "bid"
nblks = GridDim X -- variable "gridDim.x"

unblock :: Array Push a -> GlobalArray Push a 
unblock array = GlobalArray newFun (nblks * (fromIntegral n)) 
 -- from a kernel's point of view the arrays is (nblks * n) long
  where 
    (Array (Push fun) n) = array
    newFun  = Push (\func -> fun (\(i,a) -> func (bid * (fromIntegral n)+i,a)))




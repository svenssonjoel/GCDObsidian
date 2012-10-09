{-# LANGUAGE MultiParamTypeClasses, 
             FlexibleInstances,
             FlexibleContexts, 
             UndecidableInstances,  
             GADTs #-} 

module Obsidian.GCDObsidian.Array where 
    
import Obsidian.GCDObsidian.Exp 
import Obsidian.GCDObsidian.Types
import Obsidian.GCDObsidian.Globs
import Obsidian.GCDObsidian.Program

import Obsidian.GCDObsidian.Shape 

import Data.List
import Data.Word

------------------------------------------------------------------------------
-- Push and pull arrays 
data Push sh a = Push { pushShape :: (Shape sh Word32), 
                        pushFun :: P (Shape (E sh) (Exp Word32),a) }

data Pull sh a = Pull { pullShape :: (Shape sh Word32), 
                        pullFun :: (Shape (E sh) (Exp Word32) -> a) }

newtype P a = P {unP :: (a -> Program ()) -> Program ()}  


testArray1 :: Pull DIM1 (Exp Int) 
testArray1 = Pull sh  (\s -> index "apa" (toIndex sh s) ) 
    where sh = (mkShape 1000)



{- 
-- Push and Pull global arrays. 
data PushG a = PushG (Word32,Word32) {pushGFun :: P (Exp Word32,Exp Word32, a)}
data PullG a = PullG (Word32,Word32) {pullGFun :: Exp Word32 -> Exp Word32 -> a} 




type ArrayPull a = 

data Array p a = Array Word32 (p a)

-- A blocked array indexible via a blockIdx and a threadIdx
-- Static blocksize and number of blocks. 
data BArray p a = BArray (Word32,Word32) (p a)

type PushArray a = Array Push a 
type PullArray a = Array Pull a

type GlobalArray p a = BArray p a
type GlobalPushArray a = GlobalArray Push a
type GlobalPullArray a = GlobalArray Pull a 

mkPushArray n p = Array n (Push (P p)) 
mkPullArray n p = Array n (Pull p)  

resize (Array n p) m = Array m p 

runP :: P a -> (a -> Program ()) -> Program ()
runP p a = (unP p) a  

instance Monad P where 
  return a = P $ \k -> k a 
  (>>=) (P m) f = P $ \k -> m (\a -> runP  (f a) k) 

instance Functor P where 
  fmap f (P m) = P $ \k -> m (\a -> k (f a))  


-- TODO: Do you need (Exp e) where there is only e ?
class  PushyInternal a where 
  push' :: Word32 -> a e -> Array Push e  
  push'' :: Word32 -> a e -> Array Push e 

instance PushyInternal (Array Pull)  where   
  push' m (Array n (Pull ixf)) =
    mkPushArray n $ \k ->
                    ForAll (n `div` m)
                           (\i -> foldr1 (*>*)
                                [ k (ix, a)
                                | j <- [0..m-1]
                                , let ix = (i * fromIntegral m +
                                            fromIntegral j)
                                      a  = ixf ix
                                ])     

  push'' m (Array n (Pull ixf)) =
    mkPushArray n $ \k ->
                    ForAll (n `div` m)
                           (\i -> foldr1 (*>*)
                                  [ k (ix,a)
                                  | j <- [0..m-1]
                                  , let ix = 1 + ((fromIntegral ((n `div` m) * j)))
                                        a  = ixf ix
                                  ])
  
class Pushy a p e where
  push :: a p e -> a Push e

instance Pushy Array Push e where 
  push = id 
  
instance Pushy Array Pull e  where   
  push (Array n (Pull ixf)) =
    mkPushArray n $ \k ->
                    ForAll n (\i -> k (i, ixf i))
  
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

namedArray name n = mkPullArray n (\ix -> index name ix) 
indexArray n      = mkPullArray n (\ix -> ix) 

class Indexible a e where 
  access :: a e -> Exp Word32 -> e 
  
instance Indexible (Array Pull) a where
  access (Array _ ixf) ix = pullFun ixf ix

class PushApp a where 
  papp :: a e -> ((Exp Word32,e) -> Program ()) -> Program ()

instance PushApp (Array Push) where 
  papp (Array n (Push f)) a = (unP f) a 

--instance PushApp (GlobalArray Push) where 
--  papp (GlobalArray (Push f) n) a = f a 
  
class Len a where 
  len :: a e -> Word32

instance Len (Array p) where 
  len (Array n _) = n 
  
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
-}
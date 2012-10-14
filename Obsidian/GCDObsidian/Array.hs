{-# LANGUAGE MultiParamTypeClasses, 
             FlexibleInstances,
             FlexibleContexts, 
             UndecidableInstances,  
             GADTs,
             TypeOperators #-} 

module Obsidian.GCDObsidian.Array where 
    
import Obsidian.GCDObsidian.Exp hiding (Z)
import Obsidian.GCDObsidian.Types
import Obsidian.GCDObsidian.Globs
import Obsidian.GCDObsidian.Program

import Obsidian.GCDObsidian.Shape 

import Data.List
import Data.Word



------------------------------------------------------------------------------
-- more descriptive type aliases
type ArrayPull sh a = Pull sh a 
type ArrayPush sh a = Push sh a


------------------------------------------------------------------------------
newtype P a = P {unP :: (a -> Program ()) -> Program ()}  

-- Push and pull arrays 
data Push sh a = Push { pushShape :: Shape sh Word32, 
                        pushFun :: P (Shape (E sh) (Exp Word32),a) }

mkPush sh p = Push sh (P p) 

data Pull sh a = Pull { pullShape :: Shape sh Word32, 
                        pullFun   :: Shape (E sh) (Exp Word32) -> a }

data PushG bdim gdim a =
  PushG
  {
    pushGGridDim  :: Shape bdim Word32,
    pushGBlockDim :: Shape gdim Word32,
    
    pushGFun   ::  P (Shape (E bdim) (Exp Word32),
                      Shape (E gdim) (Exp Word32),
                      a)
  }

mkPushG gsh bsh p = PushG gsh bsh (P p) 

data PullG gdim bdim a =
  PullG
  {
    pullGGridDim  :: Shape gdim Word32,
    pullGBlockDim :: Shape bdim Word32,
    
    pullGFun   :: Shape (E gdim) (Exp Word32) ->
                  Shape (E bdim) (Exp Word32) ->
                  a
  }
                  

testArray1 :: Pull DIM1 (Exp Int) 
testArray1 = Pull sh  (\s -> index "apa" (toIndex sh s) ) 
    where sh = mkShape 1000


testGlobal1 :: PullG DIM1 DIM1 (Exp Int)
testGlobal1 = PullG gdim bdim
              $ \bix tix ->
                  indexG "globalArray"
                         (fromIntegral bsize)
                         (toBIndex gdim bdim bix) 
                         (toIndex bdim tix)
  where
    bsize = size bdim
    bdim = mkShape 256
    gdim = mkShape 100  -- so 25600 total elements.

-- Problem: how do I create an index into these kinds of dims.
--  I dont want to need to give these strange types explicitly :(  (see below) 
testix :: Exp Word32 
testix = toIndex dim ixinto
   where
     dim = mkShape 100 :: Shape (Z:.Word32) Word32 
     ixinto = mkIndex dim [50] -- :: Shape (Z:.Exp Word32) (Exp Word32)
     

(!) :: ArrayPull sh e -> Shape (E sh) (Exp Word32) -> e
(!) (Pull sh f) sh' = f sh' 

(!*) :: PullG gsh bsh e 
        -> (Shape (E gsh) (Exp Word32),
            Shape (E bsh) (Exp Word32))
        -> e
(!*) (PullG gsh bsh f) (bix,tix) = f bix tix 




----------------------------------------------------------------------------
-- Creating arrays 
namedArray n name  = Pull n (\ix -> index name (toIndex n ix)) 

namedGlobal gsh bsh name =
  PullG gsh bsh
        (\bix tix ->
         indexG name 
                (fromIntegral (size bsh))
                (toIndex gsh bix)
                (toIndex bsh tix)) 

----------------------------------------------------------------------------
-- Converting to push arrays 

class ToPush a where 
    toPush :: a sh e -> Push sh e 
    
instance ToPush Pull where 
    toPush (Pull sh ixf) = 
       mkPush sh $ \k -> ForAll (fromIntegral (size sh))
                              (\i ->
                                   let i' = fromIndex sh i         
                                   in k (i', ixf i'))


class ToPushG a where 
    toPushG :: a sh1 sh2 e -> PushG sh1 sh2  e 
    
instance ToPushG PullG where 
    toPushG (PullG gsh bsh ixf) = 
       mkPushG gsh bsh $ \k -> ForAllGlobal
                                 (fromIntegral (size gsh))
                                 (fromIntegral (size bsh)) 
                                (\bix tix ->
                                  let bix' = fromIndex gsh bix
                                      tix' = fromIndex bsh tix
                                  in k (bix',tix', ixf bix' tix'))



-- Need to div,mod i into the correct shape.. 
-- I think I know what works for 1D and 2D.. but nD ?
                                    
 
{- 

instance Pushy Array Pull e  where   
  push (Array n (Pull ixf)) =
    mkPushArray n $ \k ->
                    ForAll n (\i -> k (i, ixf i))




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
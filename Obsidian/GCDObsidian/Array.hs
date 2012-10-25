{-# LANGUAGE MultiParamTypeClasses, 
             FlexibleInstances,
             FlexibleContexts, 
             UndecidableInstances,  
             GADTs,
             ScopedTypeVariables,
             RankNTypes #-} 

module Obsidian.GCDObsidian.Array ((!) -- pull array apply (index into)
                                --   ,(!*) -- push array apply 
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

                                  , GlobalArray(..)
                                  , mkGlobalPushArray  
                                  , mkGlobalPullArray
                                  , Pull(..)
                                  , Push(..)
                                  )where 

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

mkPush p = Push (P p)  

data Array p a = Array Word32 (p a) 

type PushArray a = Array Push a 
type PullArray a = Array Pull a 

mkPushArray n p = Array n (Push (P p)) 
mkPullArray n p = Array n (Pull p)  

resize m (Array n p) = Array m p 



-- TODO: Do you need (Exp e) where there is only e ? 
class  PushyInternal a where 
  push' :: Word32 -> a e -> Array Push e  
  push'' :: Word32 -> a e -> Array Push e 




instance PushyInternal (Array Pull)  where   
  push' m (Array n (Pull ixf)) = 
    Array n (mkPush (\k ->
                       do
                         func <- runFunc k
                         return $ ForAll (\i -> foldr1 (*>*) 
                                   [func (ix,a)
                                   | j <-  [0..m-1],
                                     let ix = (i*(fromIntegral m) + (fromIntegral j)),
                                     let a  = ixf ix
                                   ]) (n `div` m)))
  push'' m (Array n (Pull ixf)) = 
    Array n (mkPush (\k ->
                      do
                        func <- runFunc k 
                        return $ ForAll (\i -> foldr1 (*>*) 
                                   [func (ix,a)
                                   | j <-  [0..m-1],
                                     let ix = (i+((fromIntegral ((n `div` m) * j)))),
                                     let a  = ixf ix
                                   ]) (n `div` m)))
             
class Pushy a where 
  push :: a e -> Array Push e 

instance Pushy (Array Push) where 
  push = id 
  
instance Pushy (Array Pull)  where   
  push (Array n (Pull ixf)) =
    Array n (mkPush (\k ->
                      do
                        func <- runFunc k 
                        return $ ForAll (\i -> func (i,ixf i)) n)) 


class PushGlobal a where 
  pushGlobal :: a e -> GlobalArray Push e 

instance PushGlobal (GlobalArray Pull) where 
  pushGlobal (GlobalArray n (Pull ixf))  = 
      GlobalArray n (mkPush (\k ->
                            do
                              func <- runFunc k
                              return $ ForAllGlobal
                                (\i -> func (i,ixf i)) n ))
----------------------------------------------------------------------------
--

namedArray name n = mkPullArray n (\ix -> index name ix)
indexArray n      = mkPullArray n (\ix -> ix)

class Indexible a e where 
  access :: a e -> Exp Word32 -> e 
  
instance Indexible (Array Pull) a where
  access (Array _ ixf) ix = pullFun ixf ix

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

 
-- infixl 9 !* 
-- (!*) :: PushApp a => a e -> ((Exp Word32,e) -> Program ()) -> Program ()
-- (!*) :: PushApp a => a e -> 
-- (!*) p a = papp p a 
 

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

data GlobalArray p a = GlobalArray (Exp Word32) (p a) 

mkGlobalPushArray n p  = GlobalArray n (Push p) 
mkGlobalPullArray n f  = GlobalArray n (Pull f) 

instance Functor (GlobalArray Pull) where 
  fmap f (GlobalArray n (Pull g)) = GlobalArray n (Pull (f . g)) 

instance Indexible (GlobalArray Pull) a where  
  access (GlobalArray _ ixf) ix = pullFun ixf ix
  
globLen (GlobalArray n _) = n



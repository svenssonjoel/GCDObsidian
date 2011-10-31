{-# LANGUAGE MultiParamTypeClasses, 
             FlexibleInstances,
             FlexibleContexts, 
             UndecidableInstances  #-} 

module Obsidian.GCDObsidian.Array ((!)
                                  , namedArray
                                  , indexArray
                                  , len 
                                  , Array(..)  
                                  , Pushy
                                  , PushyInternal
                                  , pushApp
                                  , push
                                  , push' -- this is more for "internal" use
                                  , ArrayP(..)
                                  , P(..)
                                  )where 

import Obsidian.GCDObsidian.Exp 
import Obsidian.GCDObsidian.Types
import Obsidian.GCDObsidian.Globs
import Obsidian.GCDObsidian.Program


import Data.List
import Data.Word

------------------------------------------------------------------------------
-- Arrays!
--data Array a = Array (Exp Word32 -> a) Word32 
data Array a = Array (Exp Word32 -> a) Word32 

-- PUSHY ARRAYS! 

type P a = (a -> Program ()) -> Program () 

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

data ArrayP a = ArrayP (P (Exp Word32, a)) Word32

--data ArrayP a = ArrayP ((Exp Word32 -> a -> Program ()) -> Program ()) Word32

pushApp (ArrayP func n) a =  func a 


-- TODO: Do you need (Exp e) where there is only e ? 
-- TODO: Will this again influence the Exp Tuples or not issue?
class Len a => PushyInternal a where 
  push' :: Word32 -> a e -> ArrayP e  

instance PushyInternal Array  where   
  push' m (Array ixf n) = 
    ArrayP (\func -> ForAll (\i -> foldr1 (*>*) 
                                   [func (ix,a)
                                   | j <-  [0..m-1],
                                     let ix = (i*(fromIntegral m) + (fromIntegral j)),
                                     let a  = ixf ix
                                   ]) (n `div` m)) n
    
         
class Len a => Pushy a where 
  push :: a e -> ArrayP e 

instance Pushy ArrayP where 
  push = id 
  
instance Pushy Array  where   
  push (Array ixf n) = ArrayP (\func -> ForAll (\i -> func (i,(ixf i))) n) n 

----------------------------------------------------------------------------
--

namedArray name n = Array (\ix -> index name ix) n 
indexArray n      = Array (\ix -> ix) n 

class Indexible a e where 
  access :: a e -> Exp Word32 -> e 
  
instance Indexible Array a where
  access (Array ixf _) ix = ixf ix

class Len a where 
  len :: a e -> Word32

instance Len Array where 
  len (Array _ n) = n 
instance Len ArrayP where   
  len (ArrayP _ n) = n
  

(!) :: Indexible a e => a e -> Exp Word32 -> e 
(!) = access


------------------------------------------------------------------------------
-- Show 

instance Show  a => Show (Array a) where
  show arr | len arr <= 10 =  "[" ++ 
                              (concat . intersperse ",") 
                              [show (arr ! (fromIntegral i)) | i <- [0..len arr-1]] ++ 
                              "]"
           | otherwise     =  "[" ++ 
                              (concat . intersperse ",") 
                              [show (arr ! (fromIntegral i)) | i <- [0..3]] ++ 
                              "...]"

         
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
                                  , targetArray
                                  , push
                                  , push' -- this is more for "internal" use
                                  , ArrayP(..)
                                  )where 

import Obsidian.GCDObsidian.Elem
import Obsidian.GCDObsidian.Tuple
import Obsidian.GCDObsidian.Exp 
import Obsidian.GCDObsidian.Types
import Obsidian.GCDObsidian.Globs
import Obsidian.GCDObsidian.Program


import Data.List
import Data.Word

------------------------------------------------------------------------------
-- Arrays!
data Array a = Array (Exp Word32 -> a) Word32 

-- PUSHY ARRAYS! 
data ArrayP a = ArrayP ((Exp Word32 -> a -> Program) -> Program) Word32

pushApp (ArrayP func n) a = func a 



-- TODO: Do you need (Exp e) where there is only e ? 
-- TODO: Will this again influence the Exp Tuples or not issue?
class PushyInternal a where 
  push' :: Word32 -> a e -> ArrayP e  

instance PushyInternal Array  where   
  push' m (Array ixf n) = 
    ArrayP (\func -> ForAll (\i -> foldr1 (*>*) 
                                   [func (i*(fromIntegral m)+
                                          (fromIntegral j))  
                                    (ixf (i*(fromIntegral m)+
                                            (fromIntegral j)))
                                                     | j<-  [0..m-1]
                                                     ]) (n `div` m)) n

         
class Pushy a where 
  push :: a e -> ArrayP e 

instance Pushy ArrayP where 
  push = id 
  
instance Pushy Array  where   
  push (Array ixf n) = ArrayP (\func -> ForAll (\i -> func i (ixf i)) n) n 

----------------------------------------------------------------------------
--

namedArray name n = Array (\ix -> index name ix) n 
indexArray n      = Array (\ix -> ix) n 

class Indexible a e where 
  access :: a e -> Exp Word32 -> e 
  
instance Indexible Array a where
  access (Array ixf _) ix = ixf ix


len :: Array a -> Word32
len (Array _ n) = n 

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

         
{-# LANGUAGE MultiParamTypeClasses, 
             FlexibleInstances, 
             FlexibleContexts, 
             UndecidableInstances #-} 

module Obsidian.GCDObsidian.Array ((!)
                                  , namedArray
                                  , indexArray
                                  , toLL
                                  , fromLL
                                  , LLArray(..)
                                  , OArray
                                  , Array(..)
                                  , staticLength 
                                  , dynamicLength 
                                  , len )where 


import Obsidian.GCDObsidian.Elem
import Obsidian.GCDObsidian.Tuple
import Obsidian.GCDObsidian.Exp 

import Data.List


------------------------------------------------------------------------------
-- Arrays!


type Dynamic = Bool 

data Array a = Array (Exp Int -> Exp a) Int 

namedArray name n = Array (\ix -> index name ix) n 
indexArray n      = Array (\ix -> ix) n 


data DArray a = DArray (Exp Int -> Exp a) (Exp Int) Int 
data LLArray a = LLArray (Exp Int -> Exp a) (Exp Int) Int Dynamic 


class OArray a e  where 
  toLL :: a e -> LLArray e 
  fromLL :: LLArray e -> a e 
    
instance OArray Array e where   
  toLL (Array ixf n) = LLArray ixf undefined n False
  fromLL (LLArray ixf _ n False) = Array ixf n 

class Indexible a e where 
  access :: a e -> Exp Int -> Exp e 
  
instance Indexible Array a where
  access (Array ixf _) ix = ixf ix
instance Indexible LLArray a where 
  access (LLArray ixf _ _ _) ix = ixf ix 
instance Indexible DArray a where 
  access (DArray ixf _ _) ix = ixf ix 

len :: Array a -> Int 
len (Array _ n) = n 

-- ixf (LLArray f _ _ _ ) = f 

staticLength :: LLArray a -> Int
staticLength (LLArray _ _ n _) = n 

dynamicLength :: LLArray a -> Exp Int 
dynamicLength (LLArray _ e _ _) = e

--(!) :: LLArray a -> Exp Int -> Exp a 
--(!) (LLArray ixf _ _ _) ix = ixf ix 

(!) :: Indexible a e => a e -> Exp Int -> Exp e 
(!) = access


------------------------------------------------------------------------------
-- Show 

instance Show (Exp a) => Show (Array a) where
  show arr | len arr <= 10 =  "[" ++ 
                              (concat . intersperse ",") 
                              [show (arr ! (fromIntegral i)) | i <- [0..len arr-1]] ++ 
                              "]"
           | otherwise     =  undefined

         
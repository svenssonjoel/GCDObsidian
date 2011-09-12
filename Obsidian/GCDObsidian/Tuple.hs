
{-# LANGUAGE TypeFamilies,  
             GADTs ,
             FlexibleInstances #-}

module Obsidian.GCDObsidian.Tuple where

import Obsidian.GCDObsidian.Elem

------------------------------------------------------------------------------
-- Tuple class
class IsTuple a where 
  type Repr a  
  from :: a -> Repr a 
  to   :: Repr a -> a 
  
------------------------------------------------------------------------------
-- Tuples and indices
infixr :.

data Tuple t a where
  Nil :: Tuple t ()
  (:.) :: Elem a => t a -> Tuple t b -> Tuple t (a,b)

data TupIx t a where 
  ZeroIx :: TupIx (s,t) s
  SuccIx :: TupIx t a -> TupIx (s,t) a
  
  
------------------------------------------------------------------------------
-- Instances
  
  

  
instance IsTuple () where   
  type Repr () = () 
  from = id 
  to   = id 


instance IsTuple (a,b) where 
  type Repr (a,b) = (a,(b,()))
  from (a,b) = (a,(b,()))
  to (a,(b,())) = (a,b) 

instance IsTuple (a,b,c) where 
  type Repr (a,b,c) = (a,(b,(c,())))
  from (a,b,c) = (a,(b,(c,())))
  to (a,(b,(c,()))) = (a,b,c) 
  
  

                       
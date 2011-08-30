

module Obsidian.GCDObsidian.Elem where

import Data.Word

class Show a => Elem a

instance Elem Bool    
instance Elem Int    
instance Elem Float
instance Elem Double
instance Elem Word8 
instance Elem Word16 
instance Elem Word32
instance Elem Word64

instance (Elem a, Elem b) => Elem (a,b) 
instance (Elem a, Elem b, Elem c) => Elem (a,b,c) 
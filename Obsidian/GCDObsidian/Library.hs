

module Obsidian.GCDObsidian.Library where 




import Obsidian.GCDObsidian.Array 
import Obsidian.GCDObsidian.Elem 
import Obsidian.GCDObsidian.Exp 
import Obsidian.GCDObsidian.Tuple



------------------------------------------------------------------------------
-- Reverse an array by indexing in it backwards
rev :: Array a -> Array a 
rev arr = Array (\ix -> arr ! ((fromIntegral (n-1)) - ix)) n 
  where 
    n = len arr
    
    

------------------------------------------------------------------------------
-- splitAt (name clashes with Prelude.splitAt 
splitAt :: Int -> Array a -> (Array a, Array a) 
splitAt n arr = (Array (\ix -> arr ! ix) n , 
                 Array (\ix -> arr ! (ix + fromIntegral n)) (len arr - n))


------------------------------------------------------------------------------
--
conc :: Elem a => (Array a, Array a) -> Array a 
conc (a1,a2) = Array (\ix -> ifThenElse (ix <* (fromIntegral n1)) 
                             (a1 ! ix) 
                             (a2 ! (ix - (fromIntegral n1)))) (n1+n2)
  where 
    n1 = len a1
    n2 = len a2 
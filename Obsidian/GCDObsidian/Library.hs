module Obsidian.GCDObsidian.Library where 

import Obsidian.GCDObsidian.Array 
import Obsidian.GCDObsidian.Elem 
import Obsidian.GCDObsidian.Exp 
import Obsidian.GCDObsidian.Tuple

import Data.Bits






arrayMap f arr = Array (\ix -> f (arr ! ix)) (len arr) 


------------------------------------------------------------------------------
-- Reverse an array by indexing in it backwards
rev :: Array a -> Array a 
rev arr = Array (\ix -> arr ! ((fromIntegral (n-1)) - ix)) n 
  where 
    n = len arr
    
------------------------------------------------------------------------------
-- splitAt (name clashes with Prelude.splitAt 
splitAt :: Integral i => i -> Array a -> (Array a, Array a) 
splitAt n arr = (Array (\ix -> arr ! ix) (fromIntegral n) , 
                 Array (\ix -> arr ! (ix + fromIntegral n)) (len arr - (fromIntegral n)))
--split :: Int -> Arr a -> (Arr a,Arr a)
--split  m arr = 
--    let n  = len arr
--        h1 = mkArr (\ix -> arr ! ix)  m
--        h2 = mkArr (\ix -> arr ! (ix + (fromIntegral m))) (n-m)
--    in  (h1,h2)

------------------------------------------------------------------------------
--
conc :: Elem a => (Array a, Array a) -> Array a 
conc (a1,a2) = Array (\ix -> ifThenElse (ix <* (fromIntegral n1)) 
                             (a1 ! ix) 
                             (a2 ! (ix - (fromIntegral n1)))) (n1+n2)
  where 
    n1 = len a1
    n2 = len a2 

--conc :: Choice a => (Arr a, Arr a) -> Arr a
--conc (arr1,arr2) = 
--    let (n,n') = (len arr1,len arr2)
--    in mkArr (\ix -> ifThenElse (ix <* fromIntegral n) 
--                                (arr1 !  ix)
--                                (arr2 !  (ix - fromIntegral n))) (n+n')
   
    
------------------------------------------------------------------------------
-- zipp unzipp
unzipp :: (Elem a , Elem b) =>  Array (a,b) -> (Array a, Array b)       
unzipp arr = (Array (\ix -> fst (untup2 (arr ! ix))) (len arr),
              Array (\ix -> snd (untup2 (arr ! ix))) (len arr))
              
zipp :: (Elem a, Elem b) => (Array a, Array b) -> Array (a,b)             
zipp (arr1,arr2) = Array (\ix -> tup2 (arr1 ! ix, arr2 ! ix)) (len arr1)
    
    
zipWith :: (Exp a -> Exp b -> Exp c) -> Array a -> Array b -> Array c
zipWith op a1 a2 = Array (\ix -> (a1 ! ix) `op` (a2 ! ix)) (len a1)
    
------------------------------------------------------------------------------    
-- twoK (untested for proper functionality) 

twoK::Int -> (Array a -> Array b) -> Array a -> Array b 
twoK 0 f = f  -- divide 0 times and apply f
twoK n f =  (\arr -> 
              let arr' = Array (\i -> (f (Array (\j -> (arr ! (g i j))) m)) ! (h i)) (lt) 
                  m    = (len arr `shiftR` n)   --pow of two           
                  g i j = i .&. (fromIntegral (complement (m-1))) .|. j  
                  h i   = i .&. (fromIntegral (nl2-1))   -- optimize 

                  nl2   = (len (f (Array (\j -> arr ! variable "X") m)))
                  lt    = nl2 `shiftL` n 
              in arr')  



------------------------------------------------------------------------------    
-- ivt (untested)

ivt :: Int -> Int -> (Array a -> Array b) -> Array a -> Array b
ivt i j f arr = Array g nl
  where
    g i1 = f (Array (\j2 -> arr ! (i1 `xor` (mask (j2 `xor` bb)))) 2) ! bb
        where bb = (i1 .&. bit) `shiftR` ij
    nl = len arr
    mask k = k  `shiftL` (ij+1) - k `shiftL` i
    bit = fromIntegral $ 2^ij
    ij = i+j

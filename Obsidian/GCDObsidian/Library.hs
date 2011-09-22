module Obsidian.GCDObsidian.Library where 

import Obsidian.GCDObsidian.Array 
import Obsidian.GCDObsidian.Elem 
import Obsidian.GCDObsidian.Exp 
import Obsidian.GCDObsidian.Tuple

import Data.Bits

import Prelude hiding (splitAt)


instance Functor Array where 
  fmap f arr = Array (\ix -> f (arr ! ix)) (len arr) 
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

halve arr = splitAt n2 arr
  where 
    n = len arr
    n2 = n `div` 2

------------------------------------------------------------------------------
--
conc :: Choice a => (Array a, Array a) -> Array a 
conc (a1,a2) = Array (\ix -> ifThenElse (ix <* (fromIntegral n1)) 
                             (a1 ! ix) 
                             (a2 ! (ix - (fromIntegral n1)))) (n1+n2)
  where 
    n1 = len a1
    n2 = len a2 

    
------------------------------------------------------------------------------
-- zipp unzipp
unzipp :: (Elem a , Elem b) =>  Array (Exp (a,b)) -> (Array (Exp a), Array (Exp b))       
unzipp arr = (Array (\ix -> fst (untup2 (arr ! ix))) (len arr),
              Array (\ix -> snd (untup2 (arr ! ix))) (len arr))
              
zipp :: (Elem a, Elem b) => (Array (Exp a), Array (Exp b)) -> Array (Exp (a,b))             
zipp (arr1,arr2) = Array (\ix -> tup2 (arr1 ! ix, arr2 ! ix)) (len arr1)
    
    
zipWith :: (Exp a -> Exp b -> Exp c) -> Array (Exp a) -> Array (Exp b) -> Array (Exp c)
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




----------------------------------------------------------------------------
-- PUSHY LIBRARY 
----------------------------------------------------------------------------

revP :: ArrayP a -> ArrayP a 
revP (ArrayP h n) = ArrayP (revHelp (\ix -> (fromIntegral (n-1)) - ix) h) n 
  where 
    revHelp f p = \func -> p (\i -> func (f i))
    
    
concP :: ArrayP a -> ArrayP a -> ArrayP a     
concP (ArrayP f n1) (ArrayP g n2) = 
  ArrayP (\func -> ProgramSeq ( f func )
                              ( g (\i -> func (fromIntegral n1 + i))))
                       (n1+n2)


concP' :: ArrayP a -> ArrayP a -> ArrayP a     
concP' (ArrayP f n1) (ArrayP g n2) = 
  case compare n1 n2 of 
    EQ -> ArrayP (\func -> ProgramSeq ( f func )
                           (g (\i -> func (fromIntegral n1 + i))))
           newlen
          
    LT -> ArrayP (\func -> ProgramSeq ( f (\i  a ->  Cond (i <* (fromIntegral n1))
                                                    (func i a )))
                           (g (\i -> func (fromIntegral n1 + i))))
           newlen
          
    GT -> ArrayP (\func -> ProgramSeq (f func ) 
                           (g (\i a -> Cond (i <* (fromIntegral n2))
                                       (func (fromIntegral n1 + i) a ))))
          newlen
  where 
    newlen = n1+n2


-- TODO: Are there cool versions of twoK and ivt on that produce 
--       Pushy Arrays
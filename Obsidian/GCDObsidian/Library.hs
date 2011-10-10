module Obsidian.GCDObsidian.Library where 

import Obsidian.GCDObsidian.Array 
import Obsidian.GCDObsidian.Elem 
import Obsidian.GCDObsidian.Exp 
import Obsidian.GCDObsidian.Tuple
import Obsidian.GCDObsidian.Program
import Obsidian.GCDObsidian.Kernel

import Data.Bits
import Data.Word

import Prelude hiding (splitAt,zipWith)


instance Functor Array where 
  fmap f arr = Array (\ix -> f (arr ! ix)) (len arr) 



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

----------------------------------------------------------------------------
-- elements at even indices to fst output, odd to snd.
evenOdds :: Array a -> (Array a, Array a)
evenOdds arr = (Array (\ix -> arr ! (2*ix)) (n-n2),
                Array (\ix -> arr ! (2*ix + 1)) n2)
  where
    n = fromIntegral (len arr)
    n2 = div n 2


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

unzipp :: Array (a,b) -> (Array a, Array b)       
unzipp arr = (Array (\ix -> fst (arr ! ix)) (len arr),
              Array (\ix -> snd (arr ! ix)) (len arr))
              
zipp :: (Array a, Array b) -> Array (a, b)             
zipp (arr1,arr2) = Array (\ix -> (arr1 ! ix, arr2 ! ix)) (len arr1)


unzipp3 :: Array (a,b,c) 
           -> (Array a, Array b, Array c)       
unzipp3 arr = (Array (\ix -> fst3 (arr ! ix)) (len arr),
               Array (\ix -> snd3 (arr ! ix)) (len arr),
               Array (\ix -> trd3 (arr ! ix)) (len arr))
  where
    fst3 (x,_,_) = x
    snd3 (_,y,_) = y
    trd3 (_,_,z) = z
    
zipp3 :: (Array a, Array b, Array c) 
         -> Array (a,b,c)             
zipp3 (arr1,arr2,arr3) = 
  Array (\ix -> (arr1 ! ix, arr2 ! ix, arr3 ! ix)) 
    (minimum [len arr1, len arr2, len arr3])


zipWith :: (a -> b -> c) -> Array a -> Array b -> Array c
zipWith op a1 a2 = Array (\ix -> (a1 ! ix) `op` (a2 ! ix)) (len a1)

                   
----------------------------------------------------------------------------
-- pair 

pair :: Array a -> Array (a,a)
pair (Array ixf n) = Array (\ix -> (ixf (ix*2),ixf (ix*2+1))) n'
  where 
    n' = n `div` 2 



------------------------------------------------------------------------------    
-- twoK (untested for proper functionality) 

twoK ::Int -> (Array a -> Array b) -> Array a -> Array b 
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


-- Split an array into two equal length parts.
-- How they are divided is controlled by i and j 
-- TODO: Ask Mary for info on the "how"
ivDiv :: Int -> Int -> Array a -> (Array a, Array a)
ivDiv i j arr = (Array (\ix -> arr ! newix0 i j ix) (n-n2),
                 Array (\ix -> arr ! newix1 i j ix) n2)
  where 
    n1 = n-n2
    n2 = div n 2
    n = len arr
    newix0 i j ix = ix + (ix .&. complement (fromIntegral (tij - 1)))
    tij = 2^(i+j)
    newix1 i j ix = (newix0 i j ix) `xor` (fromIntegral j')
    j' = (((2^(j+1))-1) :: Word32) `shiftL` i




----------------------------------------------------------------------------
-- ***                          PUSHY LIBRARY                        *** ---
----------------------------------------------------------------------------

revP :: Pushy arr => arr a -> ArrayP a 
revP  arr = ArrayP (revHelp (\ix -> (fromIntegral (n-1)) - ix) h) n 
  where
    (ArrayP h n) = push arr
--revHelp :: (a -> b) -> ((a -> c) -> d) -> (b -> c) -> d

-- TODO: This can be used in general to apply some indexing transformation.
revHelp :: (Exp Word32 -> Exp Word32) -> ((Exp Word32 -> a) -> Program ()) -> (Exp Word32 -> a) -> Program ()
revHelp f g h = g (\i -> h (f i))
    
    
--concP :: ArrayP a -> ArrayP a -> ArrayP a     
--concP (ArrayP f n1) (ArrayP g n2) = 
--  ArrayP (\func -> ProgramSeq ( f func )
--                              ( g (\i -> func (fromIntegral n1 + i))))
--                       (n1+n2)


concP :: Pushy arr => arr a -> arr a -> ArrayP a     
concP arr1 arr2 = 
  case compare n1 n2 of 
    EQ -> ArrayP (\func -> ( f func )
                           *>* 
                           (g (\i -> func (fromIntegral n1 + i))))
           newlen
          
    LT -> ArrayP (\func -> (f (\i  a ->  
                                Cond (i <* (fromIntegral n1)) 
                                (func i a )))
                           *>*
                           (g (\i -> func (fromIntegral n1 + i))))
           newlen
          
    GT -> ArrayP (\func -> (f func ) 
                           *>*
                           (g (\i a -> Cond (i <* (fromIntegral n2))
                                       (func (fromIntegral n1 + i) a ))))
          newlen
  where 
    newlen = n1+n2
    (ArrayP f n1) = push arr1
    (ArrayP g n2) = push arr2


----------------------------------------------------------------------------
-- 
    
zipP :: Pushy arr  => arr a -> arr a -> ArrayP a  
zipP arr1 arr2 =
  ArrayP (\func -> (f (\i -> func (2*i)))
                   *>*
                   (g (\i -> func (2*i + 1))))
         (n1+n2)
  where 
    (ArrayP f n1) = push arr1
    (ArrayP g n2) = push arr2
    

-- The oposite to ivDiv    
ivMerge :: Pushy arr => Int -> Int -> arr a -> arr a -> ArrayP a
ivMerge i j arr1 arr2  = 
  ArrayP (\func -> (f (\ix -> func (newix0 i j ix)))
                   *>*
                   (g (\ix -> func (newix1 i j ix))))
          (n1+n2)
  where
    newix0 i j ix = ix + (ix .&. complement (fromIntegral (tij - 1)))
    tij = 2^(i+j)
    newix1 i j ix = (newix0 i j ix) `xor` (fromIntegral j')
    j' = (((2^(j+1))-1) :: Word32) `shiftL` i
    (ArrayP f n1) = push arr1
    (ArrayP g n2) = push arr2
    
    
-- iv  a sorter building block
iv i j f g arr = part
  where
    (arr1,arr2) = ivDiv i j arr
    part = ivMerge i j arr1' arr2'
    arr1' = push $ zipWith f arr1 arr2
    arr2' = push $ zipWith g arr1 arr2


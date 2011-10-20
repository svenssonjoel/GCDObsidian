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
zipWith op a1 a2 = Array (\ix -> (a1 ! ix) `op` (a2 ! ix)) 
                   (min (len a1) (len a2))

                   
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
{-
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
-} 

-- Improved ivDiv
ivDiv :: Int -> Int -> Array a -> (Array a, Array a)
ivDiv i j (Array ixf n) = (Array (ixf . left) (n-n2),
                           Array (ixf . right) n2   )
  where 
    n2 = n `div` 2
    left = insertZero (i+j) 
    right ix = (left ix) `xor` (fromIntegral mask)
    mask = (oneBits j :: Word32) `shiftL` i


----------------------------------------------------------------------------
-- ***                          PUSHY LIBRARY                        *** ---
----------------------------------------------------------------------------

revP :: Pushy arr => arr a -> ArrayP a 
revP  arr = ixMap (\ix -> (fromIntegral (n-1)) - ix) parr -- ArrayP (ixMap (\ix -> (fromIntegral (n-1)) - ix) h) n 
  where
    parr@(ArrayP h n) = push arr


ixMap :: (Exp Word32 -> Exp Word32) 
         -> ArrayP a 
         -> ArrayP a 
ixMap f (ArrayP p n) = ArrayP (ixMap' f p) n

ixMap' :: (Exp Word32 -> Exp Word32) 
         -> P (Exp Word32, a)
         -> P (Exp Word32, a) 
ixMap' f p = \g -> p (\(i,a) -> g (f i,a))

concP :: ArrayP a -> ArrayP a -> ArrayP a     
concP (ArrayP f n1) (ArrayP g n2) = 
  ArrayP (\func -> ProgramSeq ( f func )
                              ( g (\(i,a) -> func (fromIntegral n1 + i,a))))
                       (n1+n2)

----------------------------------------------------------------------------
-- 
    
zipP :: Pushy arr  => arr a -> arr a -> ArrayP a  
zipP arr1 arr2 =
  ArrayP (\func -> (f (\(i,a) -> func (2*i,a)))
                   *>*
                   (g (\(i,a) -> func (2*i + 1,a))))
         (n1+n2)
  where 
    (ArrayP f n1) = push arr1
    (ArrayP g n2) = push arr2
    
    
    
-- Combine assumes the two push arrays     
-- Pushes to completely disjoint sets of indices     
-- and that all indices between 0 and their combined 
-- length is being pushed to. 
-- should definitely not be exposed to the outside. 
combine :: ArrayP a -> ArrayP a -> ArrayP a    
combine a1 a2 = 
  ArrayP (\k -> pushApp a1 k *>* pushApp a2 k) (len a1 + len a2) 
    
  
-- The oposite to ivDiv    
ivMerge :: Pushy arr => Int -> Int -> arr a -> arr a -> ArrayP a
ivMerge i j arr1 arr2 = combine a1 a2
  where
    left ix = ix + (ix .&. complement (oneBits (i+j)))
    right ix = (left ix) `xor` (fromIntegral mask)
    mask = (oneBits j :: Word32) `shiftL` i
    a1 = ixMap left (push arr1)
    a2 = ixMap right (push arr2)
   
   

    
-- iv  a sorter building block
iv i j f g arr = ivMerge i j arr1' arr2'
  where
    (arr1,arr2) = ivDiv i j arr
    arr1' = push $ zipWith f arr1 arr2
    arr2' = push $ zipWith g arr1 arr2


insertZero :: Int -> Exp Word32 -> Exp Word32
insertZero i a = a + (a .&. fromIntegral (complement (oneBits i :: Word32)))

flipBits :: Bits a => Int -> Int -> a -> a
flipBits i j a = a `xor` (fromIntegral mask)
  where
    mask = (oneBits j :: Word32) `shiftL` i



oneBits i = bit i - 1
    
    
    
    
{-
-- The oposite to ivDiv    
    
ivMerge :: Pushy arr => Int -> Int -> arr a -> arr a -> ArrayP a
ivMerge i j arr1 arr2  = 
  ArrayP (\func -> (f (\(ix,a) -> func (newix0 i j ix,a)))
                   *>*
                   (g (\(ix,a) -> func (newix1 i j ix,a))))
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

-} 


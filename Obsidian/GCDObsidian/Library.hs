{-# LANGUAGE FlexibleInstances #-}
module Obsidian.GCDObsidian.Library where 

import Obsidian.GCDObsidian.Array 
import Obsidian.GCDObsidian.Exp 
import Obsidian.GCDObsidian.Program
import Obsidian.GCDObsidian.Kernel

import Data.Bits
import Data.Word

import Prelude hiding (splitAt,zipWith)



instance Functor (Array Pull) where 
  fmap f arr = Array (Pull (\ix -> f (arr ! ix))) (len arr) 

------------------------------------------------------------------------------
-- Reverse an array by indexing in it backwards
rev :: Array Pull a -> Array Pull a 
rev arr = Array (Pull (\ix -> arr ! ((fromIntegral (n-1)) - ix))) n 
  where 
    n = len arr
    
------------------------------------------------------------------------------
-- splitAt (name clashes with Prelude.splitAt 
splitAt :: Integral i => i -> Array Pull a -> (Array Pull a, Array Pull a) 
splitAt n arr = (Array (Pull (\ix -> arr ! ix)) (fromIntegral n) , 
                 Array (Pull (\ix -> arr ! (ix + fromIntegral n))) (len arr - (fromIntegral n)))



halve arr = splitAt n2 arr
  where 
    n = len arr
    n2 = n `div` 2

----------------------------------------------------------------------------
-- elements at even indices to fst output, odd to snd.
evenOdds :: Array Pull a -> (Array Pull a, Array Pull a)
evenOdds arr = (Array (Pull (\ix -> arr ! (2*ix))) (n-n2),
                Array (Pull (\ix -> arr ! (2*ix + 1))) n2)
  where
    n = fromIntegral (len arr)
    n2 = div n 2


------------------------------------------------------------------------------
--
conc :: Choice a => (Array Pull a, Array Pull a) -> Array Pull a 
conc (a1,a2) = Array (Pull (\ix -> ifThenElse (ix <* (fromIntegral n1)) 
                             (a1 ! ix) 
                             (a2 ! (ix - (fromIntegral n1))))) (n1+n2)
  where 
    n1 = len a1
    n2 = len a2 

    
------------------------------------------------------------------------------
-- zipp unzipp

unzipp :: Array Pull (a,b) -> (Array Pull a, Array Pull b)       
unzipp arr = (Array (Pull (\ix -> fst (arr ! ix))) (len arr),
              Array (Pull (\ix -> snd (arr ! ix))) (len arr))
              
zipp :: (Array Pull a, Array Pull b) -> Array Pull (a, b)             
zipp (arr1,arr2) = Array (Pull (\ix -> (arr1 ! ix, arr2 ! ix))) (min (len arr1) (len arr2))


unzipp3 :: Array Pull (a,b,c) 
           -> (Array Pull a, Array Pull b, Array Pull c)       
unzipp3 arr = (Array (Pull (\ix -> fst3 (arr ! ix))) (len arr),
               Array (Pull (\ix -> snd3 (arr ! ix))) (len arr),
               Array (Pull (\ix -> trd3 (arr ! ix))) (len arr))
  where
    fst3 (x,_,_) = x
    snd3 (_,y,_) = y
    trd3 (_,_,z) = z
    
zipp3 :: (Array Pull a, Array Pull b, Array Pull c) 
         -> Array Pull (a,b,c)             
zipp3 (arr1,arr2,arr3) = 
  Array (Pull (\ix -> (arr1 ! ix, arr2 ! ix, arr3 ! ix)))
    (minimum [len arr1, len arr2, len arr3])


zipWith :: (a -> b -> c) -> Array Pull a -> Array Pull b -> Array Pull c
zipWith op a1 a2 = Array (Pull (\ix -> (a1 ! ix) `op` (a2 ! ix))) 
                   (min (len a1) (len a2))

                   
----------------------------------------------------------------------------
-- pair 

pair :: Array Pull a -> Array Pull (a,a)
pair (Array (Pull ixf) n) = Array (Pull (\ix -> (ixf (ix*2),ixf (ix*2+1)))) n'
  where 
    n' = n `div` 2 



unpair :: Choice a => Array Pull (a,a) -> Array Pull a
unpair arr = 
    let n = len arr
    in  Array (Pull (\ix -> ifThenElse ((mod ix 2) ==* 0) 
                      (fst (arr ! (ix `shiftR` 1)))
                      (snd (arr ! (ix `shiftR` 1))))) (2*n)


------------------------------------------------------------------------------    
-- twoK (untested for proper functionality) 

twoK ::Int -> (Array Pull a -> Array Pull b) -> Array Pull a -> Array Pull b 
twoK 0 f = f  -- divide 0 times and apply f
twoK n f =  (\arr -> 
              let arr' = Array (Pull (\i -> (f (Array (Pull (\j -> (arr ! (g i j)))) m) ! (h i)))) lt
                  m    = (len arr `shiftR` n)   --pow of two           
                  g i j = i .&. (fromIntegral (complement (m-1))) .|. j  
                  h i   = i .&. (fromIntegral (nl2-1))   -- optimize 

                  nl2   = (len (f (Array (Pull (\j -> arr ! variable "X")) m)))
                  lt    = nl2 `shiftL` n 
              in arr')  



------------------------------------------------------------------------------    
-- ivt (untested)

ivt :: Int -> Int -> (Array Pull a -> Array Pull b) -> Array Pull a -> Array Pull b
ivt i j f arr = Array (Pull g) nl
  where
    g i1 = f (Array (Pull (\j2 -> arr ! (i1 `xor` (mask (j2 `xor` bb))))) 2) ! bb
        where bb = (i1 .&. bit) `shiftR` ij
    nl = len arr
    mask k = k  `shiftL` (ij+1) - k `shiftL` i
    bit = fromIntegral $ 2^ij
    ij = i+j


------------------------------------------------------------------------------    
-- Improved ivDiv    
ivDiv :: Int -> Int -> Array Pull a -> (Array Pull a, Array Pull a)
ivDiv i j (Array (Pull ixf) n) = (Array (Pull (ixf . left)) (n-n2),
                                  Array (Pull (ixf . right)) n2   )
  where 
    n2 = n `div` 2
    left = insertZero (i+j) 
    right ix = (left ix) `xor` (fromIntegral mask)
    mask = (oneBits (j+1) :: Word32) `shiftL` i


----------------------------------------------------------------------------
-- ***                          PUSHY LIBRARY                        *** ---
----------------------------------------------------------------------------

revP :: Pushy arr => arr a -> Array Push a 
revP  arr = ixMap (\ix -> (fromIntegral (n-1)) - ix) parr -- ArrayP (ixMap (\ix -> (fromIntegral (n-1)) - ix) h) n 
  where
    parr@(Array (Push h) n) = push arr


ixMap :: (Exp Word32 -> Exp Word32) 
         -> Array Push a 
         -> Array Push a 
ixMap f (Array (Push p) n) = Array (Push (ixMap' f p)) n

ixMap' :: (Exp Word32 -> Exp Word32) 
         -> P (Exp Word32, a)
         -> P (Exp Word32, a) 
ixMap' f p = \g -> ( p) (\(i,a) -> g (f i,a))

concP :: (Pushy arr1,
          Pushy arr2) => (arr1 a, arr2 a) -> Array Push a     
concP (arr1,arr2) = 
  Array (Push (\func -> f func
                   *>* 
                   g (\(i,a) -> func (fromIntegral n1 + i,a))))
                         (n1+n2)
  where 
     Array (Push f) n1 = push arr1
     Array (Push g) n2 = push arr2
     
unpairP :: Pushy arr => arr (a,a) -> Array Push a 
unpairP arr =  Array (Push (\k -> f (everyOther k)))
         (2 * n)
  where 
    Array (Push f) n = push arr 
        
everyOther :: ((Exp Word32, a) -> Program ()) 
              -> (Exp Word32, (a,a)) -> Program ()
everyOther f  = \(ix,(a,b)) -> f (ix * 2,a) *>* f (ix * 2 + 1,b)  
    
----------------------------------------------------------------------------
-- 
    
zipP :: Pushy arr  => arr a -> arr a -> Array Push a  
zipP arr1 arr2 =
  Array (Push (\func -> (f (\(i,a) -> func (2*i,a)))
                   *>*
                   (g (\(i,a) -> func (2*i + 1,a)))))
         (n1+n2)
  where 
    (Array (Push f) n1) = push arr1
    (Array (Push g) n2) = push arr2
    
    
    
-- Combine assumes the two push arrays     
-- Pushes to completely disjoint sets of indices     
-- and that all indices between 0 and their combined 
-- length is being pushed to. 
-- Should definitely not be exposed to the outside. 
combine :: Array Push a -> Array Push a -> Array Push a    
combine (Array a1 n1) (Array a2 n2) = 
  Array (Push (\k -> pushFun a1 k *>* pushFun a2 k)) (n1 + n2) 
    
  
  
----------------------------------------------------------------------------  
-- The oposite to ivDiv    
ivMerge :: Pushy arr => Int -> Int -> arr a -> arr a -> Array Push a
ivMerge i j arr1 arr2 = Array (Push (\k -> app a1 k *>* app a2 k)) (len a1 + len a2) 
  where
    left ix = ix + (ix .&. complement (oneBits (i+j)))
    right ix = (left ix) `xor` (fromIntegral mask)
    mask = (oneBits (j+1) :: Word32) `shiftL` i
    a1 = ixMap left (push arr1)
    a2 = ixMap right (push arr2)
    app (Array f _) a = pushFun f a 
  
----------------------------------------------------------------------------
-- iv  a sorter building block
iv i j f g arr = ivMerge i j arr1' arr2'
  where
    (arr1,arr2) = ivDiv i j arr
    arr1' = push $ zipWith f arr1 arr2
    arr2' = push $ zipWith g arr1 arr2


-- Stuff added or changed by Mary

insertZero :: Int -> Exp Word32 -> Exp Word32
insertZero 0 a = a `shiftL` 1
insertZero i a = a + (a .&. fromIntegral (complement (oneBits i :: Word32)))



flipBits :: Bits a => Int -> Int -> a -> a
flipBits i j a = a `xor` (fromIntegral mask)
  where
    mask = (oneBits j :: Word32) `shiftL` i

flipBit :: Bits a => Int -> a -> a
flipBit = flip complementBit 

oneBits :: Bits a => Int -> a
oneBits i = bit i - 1
    
-- flip bits from position i to position i+j inclusive
flipBitsFrom :: Bits a => Int -> Int -> a -> a
flipBitsFrom i j a = a `xor` (fromIntegral mask)
  where
    mask = (oneBits (j + 1):: Word32) `shiftL` i
     

lowBit :: Int -> UWordE -> Exp Bool
lowBit i ix = (ix .&. bit i) ==* 0

flipLSBsTo :: Int -> UWordE -> UWordE
flipLSBsTo i = (`xor` (oneBits (i+1)))



  

ilv1 :: Choice a => Int -> (b -> b-> a) -> (b -> b -> a) -> Array Pull b -> Array Pull a
ilv1 i f g arr = Array (Pull ixf) (len arr)
  where
    ixf ix = let l = arr ! ix
                 r = arr ! newix
                 newix = flipBit i ix
             in (ifThenElse (lowBit i ix) (f l r) (g l r))

vee1 :: Choice a => Int -> (b -> b-> a) -> (b -> b -> a) -> Array Pull b -> Array Pull a
vee1 i f g arr = Array (Pull ixf) (len arr)
  where
    ixf ix = let l = arr ! ix
                 r = arr ! newix
                 newix = flipLSBsTo i ix
             in (ifThenElse (lowBit i ix) (f l r) (g l r))

ilvVee1 :: Choice a => Int -> Int -> (b -> b-> a) -> (b -> b -> a) -> Array Pull b -> Array Pull a
ilvVee1 i j f g arr = Array (Pull ixf) (len arr)
  where
    ixf ix = let l = arr ! ix
                 r = arr ! newix
                 newix = flipBitsFrom i j ix
             in (ifThenElse (lowBit (i+j) ix) (f l r) (g l r))


    



ilv2 :: Choice b => Int -> (a -> a -> b) -> (a -> a -> b) -> 
                    Array Pull a -> Array Push b
ilv2 i f g (Array (Pull ixf) n) 
   = Array (Push (\k -> app a5 k *>* app a6 k)) n
  where
    n2 = n `div` 2
    a1 = Array (Pull (ixf . left)) (n-n2)
    a2 = Array (Pull (ixf . right)) n2
    a3 = zipWith f a1 a2
    a4 = zipWith g a1 a2
    a5 = ixMap left (push a3)
    a6 = ixMap right (push a4)
    left = insertZero i
    right = flipBit i  . left
    app (Array (Push f) _) a = f a






vee2 :: Choice b => Int -> (a -> a -> b) -> (a -> a -> b) -> 
                    Array Pull a -> Array Push b
vee2 i f g (Array (Pull ixf) n) 
   = Array (Push (\k -> app a5 k *>* app a6 k)) n
  where
    n2 = n `div` 2
    a1 = Array (Pull (ixf . left)) (n-n2)
    a2 = Array (Pull (ixf . right)) n2
    a3 = zipWith f a1 a2
    a4 = zipWith g a1 a2
    a5 = ixMap left (push a3)
    a6 = ixMap right (push a4)
    left = insertZero i
    right = flipLSBsTo i  . left
    app (Array (Push f) _) a = f a

   

ilvVee2 :: Choice b => Int -> Int -> (a -> a -> b) -> (a -> a -> b) -> 
            Array Pull a -> Array Push b
ilvVee2 i j f g (Array (Pull ixf) n) 
   = Array (Push (\k -> app a5 k *>* app a6 k)) n
  where
    n2 = n `div` 2
    a1 = Array (Pull (ixf . left)) (n-n2)
    a2 = Array (Pull (ixf . right)) n2
    a3 = zipWith f a1 a2
    a4 = zipWith g a1 a2
    a5 = ixMap left (push a3)
    a6 = ixMap right (push a4)
    left = insertZero (i+j)
    right = flipBitsFrom i j . left
    app (Array (Push f) _) a = f a

----------------------------------------------------------------------------
-- Old
{- 
insertZero :: Int -> Exp Word32 -> Exp Word32
insertZero i a = a + (a .&. fromIntegral (complement (oneBits i :: Word32)))

flipBits :: Bits a => Int -> Int -> a -> a
flipBits i j a = a `xor` (fromIntegral mask)
  where
    mask = (oneBits j :: Word32) `shiftL` i



oneBits i = bit i - 1
    
    
    
    

-- The oposite to ivDiv    

    
{-    
-- iv  a sorter building block
iv i j f g arr = part
  where
    (arr1,arr2) = ivDiv i j arr
    part = ivMerge i j arr1' arr2'
    arr1' = push $ zipWith f arr1 arr2
    arr2' = push $ zipWith g arr1 arr2

-} 

-}
{-# LANGUAGE GADTs, 
             TypeFamilies,  
             RankNTypes,
             FlexibleContexts,
             FlexibleInstances, 
             UndecidableInstances #-} 

module Obsidian.GCDObsidian.Exp where 

import Data.List
import Data.Word 
import Data.Bits

import qualified Foreign.Storable as Storable

------------------------------------------------------------------------------
-- Obsidian imports
import qualified Obsidian.GCDObsidian.Tuple as Tuple 
import Obsidian.GCDObsidian.Tuple (Tuple ((:.),Nil) ) 
import Obsidian.GCDObsidian.Elem
import Obsidian.GCDObsidian.Types
import Obsidian.GCDObsidian.Globs

------------------------------------------------------------------------------
-- some synonyms
type Data a = Exp a 

type IntE    = Exp Int      
type FloatE  = Exp Float  
type DoubleE = Exp Double 
type BoolE   = Exp Bool    
type UByteE  = Exp Word8   
type UShortE = Exp Word16 
type UWordE  = Exp Word32 
type ULongE  = Exp Word64 

------------------------------------------------------------------------------
-- Class Scalar. (Things that are not tuples) 
class Elem a => Scalar a where 
  sizeOf :: Exp a -> Int   --  
  typeOf :: Exp a -> Type  --   Good enough for me ... 


instance Scalar Bool where 
  --sizeOf _ = Storable.sizeOf (undefined :: Int)
  sizeOf _ = 4
  typeOf _ = Bool 

instance Scalar Int where 
  --sizeOf _ = Storable.sizeOf (undefined :: Int)
  sizeOf _ = 4
  typeOf _ = Int
  
instance Scalar Float where
  sizeOf _ = Storable.sizeOf (undefined :: Float)
  typeOf _ = Float

  
instance Scalar Double where 
  sizeOf _ = Storable.sizeOf (undefined :: Double) 
  typeOf _ = Double

  
instance Scalar Word8 where
  sizeOf _ = 1
  typeOf _ = Word8 

  
instance Scalar Word16 where 
  sizeOf _ = 2
  typeOf _ = Word16

  
instance Scalar Word32 where 
  sizeOf _ = 4 
  typeOf _ = Word32

  
instance Scalar Word64 where 
  sizeOf _ = 8 
  typeOf _ = Word64

------------------------------------------------------------------------------ 
-- Expressions 
data Exp a where
  Literal :: Scalar a 
             => a 
             -> Exp a 

  Index   :: Scalar a => 
             (Name,[Exp Word32]) 
             -> Exp a 
             
             
  Op      :: (Elem a, Elem b) 
             => Op (a -> b) 
             -> Exp a 
             -> Exp b 
  
  Tuple   :: (Elem a, Tuple.IsTuple a) 
             => Tuple.Tuple Exp (Tuple.Repr a) 
             -> Exp a  
             
             
  Prj     :: (Elem a, Elem b, Tuple.IsTuple a) 
             => Tuple.TupIx (Tuple.Repr a) b 
             -> Exp a 
             -> Exp b
             
------------------------------------------------------------------------------
-- Operations

data Op a where 
  Add :: Num a => Op ((a,a) -> a) 
  Sub :: Num a => Op ((a,a) -> a) 
  Mul :: Num a => Op ((a,a) -> a) 
  Div :: Num a => Op ((a,a) -> a) 
  If  :: Op ((Bool,a,a) -> a) 
  
  -- Trig
  Sin :: Floating a => Op (a -> a) 
  Cos :: Floating a => Op (a -> a)
  
  -- Comparisons
  Eq  :: Ord a => Op ((a,a) -> Bool)
  Lt  :: Ord a => Op ((a,a) -> Bool) 
  LEq :: Ord a => Op ((a,a) -> Bool) 
  Gt  :: Ord a => Op ((a,a) -> Bool) 
  GEq :: Ord a => Op ((a,a) -> Bool) 
  
  -- Bitwise 
  BitwiseAnd :: Bits a => Op ((a,a) -> a) 
  BitwiseOr  :: Bits a => Op ((a,a) -> a)
  BitwiseXor :: Bits a => Op ((a,a) -> a) 
  BitwiseNeg :: Bits a => Op (a -> a) 
  ShiftL     :: Bits a => Op ((a, Int) -> a) 
  ShiftR     :: Bits a => Op ((a, Int) -> a) 
  
  -- built-ins
  Min        :: Ord a => Op ((a,a) -> a) 
  Max        :: Ord a => Op ((a,a) -> a) 


------------------------------------------------------------------------------
-- helpers 

variable name = Index (name,[])
index name ix = Index (name,[ix])


------------------------------------------------------------------------------
-- Create Tuples
tup2 :: (Elem a, Elem b) => (Exp a, Exp b) -> Exp (a,b) 
tup2 (a,b) = Tuple (a :. b :. Nil )

tup3 :: (Elem a, Elem b, Elem c) => (Exp a, Exp b, Exp c) -> Exp (a,b,c) 
tup3 (a,b,c) = Tuple (a :. b :. c :. Nil )



untup2 :: (Elem a, Elem b, Elem r, 
           Tuple.Repr r ~ (a,(b,())), Tuple.IsTuple r) => 
          Exp r -> (Exp a, Exp b) 
untup2 (Tuple (a :. b :. Nil)) = (a,b) 

untup3 :: (Elem a, Elem b, Elem c, Elem r, 
           Tuple.Repr r ~ (a,(b,(c,()))), Tuple.IsTuple r) => 
          Exp r -> (Exp a, Exp b, Exp c) 
untup3 (Tuple (a :. b :. c :. Nil)) = (a,b,c) 

------------------------------------------------------------------------------
-- Collect array names

collectArrays :: Exp a -> [Name]
collectArrays (Literal _) = []
collectArrays (Index (name,[])) = []
collectArrays (Index (name,_)) = [name]
collectArrays (Op _ t) = collectArrays t 
collectArrays (Tuple t) = collectArraysTup t 
collectArrays (Prj t e) = collectArraysPrj t e 

collectArraysTup :: forall t. Tuple.Tuple Exp t -> [String]
collectArraysTup Nil = []
collectArraysTup (a :. t) = collectArrays a ++ (collectArraysTup t) 
  
collectArraysPrj = undefined 



------------------------------------------------------------------------------
-- 
  
instance Elem a => Show (Exp a) where 
  show = printExp 

instance Elem a => Eq (Exp a) where 
  (==) = undefined 

instance (Elem a, Ord a) => Ord (Exp a) where 
    min a b = Op Min (tup2 (a,b))
    max a b = Op Max (tup2 (a,b))

------------------------------------------------------------------------------
-- INT Instances 
instance Num (Exp Int) where 
  (+) a (Literal 0) = a
  (+) (Literal 0) a = a
  (+) (Literal a) (Literal b) = Literal (a+b)
  (+) a b = Op Add (tup2 (a,b))  
  
  (-) a (Literal 0) = a 
  (-) (Literal a) (Literal b) = Literal (a - b) 
  (-) a b = Op Sub (tup2 (a,b)) 
  
  (*) a (Literal 1) = a 
  (*) (Literal 1) a = a
  (*) a b = Op Mul (tup2 (a,b)) 
  
  signum = undefined 
  abs = undefined
  fromInteger a = Literal (fromInteger a) 
  
  
instance Bits (Exp Int) where 
  (.&.) a b = Op BitwiseAnd (tup2 (a,b))
  (.|.) a b = Op BitwiseOr  (tup2 (a,b))
  xor   a b = Op BitwiseXor (tup2 (a,b)) 
  
  --TODO: See that this is not breaking something (32/64 bit, CUDA/Haskell)
  complement (Literal i) = Literal (complement i)
  
  complement a = Op BitwiseNeg a
  shiftL a i = Op ShiftL (tup2 (a,Literal i))
  shiftR a i = Op ShiftR (tup2 (a,Literal i))
  bitSize a  = sizeOf a * 8
  isSigned a = True

------------------------------------------------------------------------------
-- Word32 Instances 
instance Num (Exp Word32) where 
  (+) a (Literal 0) = a
  (+) (Literal 0) a = a
  (+) (Literal a) (Literal b) = Literal (a+b)
  (+) a b = Op Add (tup2 (a,b))  
  
  (-) a (Literal 0) = a 
  (-) (Literal a) (Literal b) = Literal (a - b) 
  (-) a b = Op Sub (tup2 (a,b)) 
  
  (*) a (Literal 1) = a 
  (*) (Literal 1) a = a
  (*) a b = Op Mul (tup2 (a,b)) 
  
  signum = undefined 
  abs = undefined
  fromInteger a = Literal (fromInteger a) 
  
  
instance Bits (Exp Word32) where 
  (.&.) a b = Op BitwiseAnd (tup2 (a,b))
  (.|.) a b = Op BitwiseOr  (tup2 (a,b))
  xor   a b = Op BitwiseXor (tup2 (a,b)) 
  complement (Literal i) = Literal (complement i) 
  complement a = Op BitwiseNeg a
  shiftL a i = Op ShiftL (tup2 (a,Literal i))
  shiftR a i = Op ShiftR (tup2 (a,Literal i))
  bitSize a  = 32
  isSigned a = False

  
  
------------------------------------------------------------------------------  
  
(==*) (Literal a) (Literal b) = Literal (a == b) 
(==*) a b = Op Eq $ tup2 (a,b)
(<*)  (Literal a) (Literal b) = Literal (a < b) 
(<*)  a b = Op Lt  $ tup2 (a,b)
(<=*) (Literal a) (Literal b) = Literal (a <= b) 
(<=*) a b = Op LEq $ tup2 (a,b)
(>*)  a b = Op Gt  $ tup2 (a,b)
(>=*) a b = Op GEq $ tup2 (a,b)


class Choice a where 
  ifThenElse :: Exp Bool -> a -> a -> a 

instance Elem a => Choice (Exp a) where  
  ifThenElse (Literal False) e1 e2 = e2
  ifThenElse (Literal True)  e1 e2 = e1
  ifThenElse b e1 e2 = Op If $ tup3 (b,e1,e2)

------------------------------------------------------------------------------ 
-- Built-ins


------------------------------------------------------------------------------
-- PrintExpressions... 

------------------------------------------------------------------------------
-- Print Expressions

printExp :: Elem a => Exp a -> String
printExp (Literal a) = show a 
printExp (Index (name,[])) = name
printExp (Index (name,es)) = 
  name ++ "[" ++ ((concat . intersperse "," . map printExp) es) ++ "]"
printExp (Op op e) = "(" ++ printOp op  ++ printExp e ++ ")"
printExp (Tuple t) = printTup t
printExp (Prj i t) = printPrj i t


printOp :: Op a -> String
printOp Add = " + " 
printOp Sub = " - " 
printOp Mul = " * "

printOp If  = " if "

printOp Eq  = " == "
printOp Lt  = " < " 
printOp LEq = " <= " 
printOp Gt  = " > "
printOp GEq = " >= " 

printOp Min = " Min "
printOp Max = " Max " 

printOp Sin = " Sin " 
printOp Cos = " Cos "

printOp BitwiseAnd = " & "
printOp BitwiseOr  = " | " 
printOp BitwiseXor = " ^ " 
printOp BitwiseNeg = " ~ "  

printTup t = "(" ++ (concat . intersperse "," . printTup') t ++ ")" 

printTup' :: forall t. Tuple.Tuple Exp t -> [String]
printTup' Nil = []
printTup' (a :. t) = printExp a : (printTup' t) 
  
                     
-- TODO: I have never used a projection...                      
printPrj = undefined 



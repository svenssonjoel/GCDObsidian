{-# LANGUAGE GADTs, 
             TypeFamilies,  
             RankNTypes,
             FlexibleContexts,
             FlexibleInstances, 
             UndecidableInstances #-} 

module Obsidian.GCDObsidian.Exp 
       (module Obsidian.GCDObsidian.Exp,
        module Obsidian.GCDObsidian.DimSpec) where 



import Data.List
import Data.Word 
import Data.Bits

import qualified Foreign.Storable as Storable

import Obsidian.GCDObsidian.DimSpec

------------------------------------------------------------------------------
-- Obsidian imports
import Obsidian.GCDObsidian.Types
import Obsidian.GCDObsidian.Globs

import Obsidian.GCDObsidian.CodeGen.SPMDC

------------------------------------------------------------------------------
-- some synonyms
-- type Data a = Exp a 

type IntE    = Exp Int      
type FloatE  = Exp Float  
type DoubleE = Exp Double 
type BoolE   = Exp Bool    
type UByteE  = Exp Word8   
type UShortE = Exp Word16 
type UWordE  = Exp Word32 
type ULongE  = Exp Word64 
------------------------------------------------------------------------------
-- Class Scalar. All the things we can handle code generation for 

class (ExpToCExp a, Show a) => Scalar a where 
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



----------------------------------------------------------------------------
-- Expressions 
data Exp a where
  Literal :: Scalar a 
             => a 
             -> Exp a 
  
  BlockIdx  :: Exp Word32
  ThreadIdx :: Exp Word32  

  Index   :: Scalar a => 
             (Name,[Exp Word32]) 
             -> Exp a

  IndexGlobal :: Scalar a
                 => Name
                 -> Exp Word32 -- BlockSize
                 -> Exp Word32 -- BlockIdx
                 -> Exp Word32 -- ThreadIdx
                 -> Exp a 
             
  If      :: Scalar a 
             => Exp Bool
             -> Exp a 
             -> Exp a 
             -> Exp a 
                          
  BinOp   :: (Scalar a,
              Scalar b, 
              Scalar c) 
             => Op ((a,b) -> c) 
             -> Exp a 
             -> Exp b 
             -> Exp c 
             
  UnOp    :: (Scalar a, 
              Scalar b)
             => Op (a -> b)            
             -> Exp a 
             -> Exp b 
             

  
----------------------------------------------------------------------------
-- Operations

data Op a where 
  Add :: Num a => Op ((a,a) -> a) 
  Sub :: Num a => Op ((a,a) -> a) 
  Mul :: Num a => Op ((a,a) -> a) 
  Div :: Num a => Op ((a,a) -> a) 
  -- If  :: Op ((Bool,a,a) -> a) 
  
  Mod :: Integral a => Op ((a,a) -> a)
         
  -- Trig
  Sin :: Floating a => Op (a -> a) 
  Cos :: Floating a => Op (a -> a)
  
  -- Comparisons
  Eq  :: Ord a => Op ((a,a) -> Bool)
  NotEq :: Ord a => Op ((a,a) -> Bool) 
  Lt  :: Ord a => Op ((a,a) -> Bool) 
  LEq :: Ord a => Op ((a,a) -> Bool) 
  Gt  :: Ord a => Op ((a,a) -> Bool) 
  GEq :: Ord a => Op ((a,a) -> Bool) 
  
  -- Boolean 
  And :: Op ((Bool,Bool) -> Bool) 
  Or  :: Op ((Bool,Bool) -> Bool)
  
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

  -- Floating (different CUDA functions for float and double, issue maybe?) 
  Exp :: Floating a => Op (a -> a) 
  Sqrt :: Floating a => Op (a -> a) 
  --RSqrt :: Floating a => Op (a -> a)  
  Log :: Floating a => Op (a -> a) 
  Log2 :: Floating a => Op (a -> a) 
  Log10 :: Floating a => Op (a -> a)  
  Pow :: Floating a => Op ((a, a) -> a)  
  -- Floating Trig
  Tan :: Floating a => Op (a -> a) 
  ASin :: Floating a => Op (a -> a) 
  ATan :: Floating a => Op (a -> a) 
  ACos :: Floating a => Op (a -> a) 
  SinH :: Floating a => Op (a -> a) 
  TanH :: Floating a => Op (a -> a) 
  CosH :: Floating a => Op (a -> a) 
  ASinH :: Floating a => Op (a -> a) 
  ATanH :: Floating a => Op (a -> a) 
  ACosH :: Floating a => Op (a -> a) 
  -- There is no "div" in "Num" but it's already defined above. 
  FDiv :: Floating a => Op ((a, a) -> a) 




------------------------------------------------------------------------------
-- helpers 

variable name = Index (name,[])
index name ix = Index (name,[ix])

indexG name bsize bix tix = IndexGlobal name bsize bix tix

                
------------------------------------------------------------------------------
-- Collect array names

collectArrays :: Scalar a => Exp a -> [Name]
collectArrays (Literal _) = []
collectArrays (Index (name,[])) = []
collectArrays (Index (name,_)) = [name]
collectArrays (BinOp _ e1 e2) = collectArrays e1 ++ collectArrays e2
collectArrays (UnOp  _ e) = collectArrays e
collectArrays (If b e1 e2) = collectArrays b ++ 
                             collectArrays e1 ++ 
                             collectArrays e2

collectArrayIndexPairs :: Scalar a => Exp a -> [(Name,Exp Word32)]
collectArrayIndexPairs (Literal _) = []
collectArrayIndexPairs (Index (name,[])) = []
collectArrayIndexPairs (Index (name,[ix])) = [(name,ix)]
collectArrayIndexPairs (BinOp _ e1 e2) = collectArrayIndexPairs e1 ++ collectArrayIndexPairs e2
collectArrayIndexPairs (UnOp  _ e) = collectArrayIndexPairs e
collectArrayIndexPairs (If b e1 e2) = collectArrayIndexPairs b ++ 
                                      collectArrayIndexPairs e1 ++ 
                                      collectArrayIndexPairs e2

------------------------------------------------------------------------------
-- 
  
instance Scalar a => Show (Exp a) where 
  show = printExp 

instance Scalar a => Eq (Exp a) where 
  (==) = undefined 

instance (Scalar a, Ord a) => Ord (Exp a) where 
    min a b = BinOp Min a b
    max a b = BinOp Max a b

------------------------------------------------------------------------------
-- INT Instances 
instance Num (Exp Int) where 
  (+) a (Literal 0) = a
  (+) (Literal 0) a = a
  (+) (Literal a) (Literal b) = Literal (a+b)
  (+) a b = BinOp Add a b  
  
  (-) a (Literal 0) = a 
  (-) (Literal a) (Literal b) = Literal (a - b) 
  (-) a b = BinOp Sub a b 
  
  (*) a (Literal 1) = a 
  (*) (Literal 1) a = a
  (*) a b = BinOp Mul a b 
  
  signum = undefined 
  abs = undefined
  fromInteger a = Literal (fromInteger a) 
  
-- Added new cases for literal 0 (2012/09/25)
instance Bits (Exp Int) where  
  (.&.) x (Literal 0) = Literal 0
  (.&.) (Literal 0) x = Literal 0 
  (.&.) (Literal a) (Literal b) = Literal (a .&. b) 
  (.&.) a b = BinOp BitwiseAnd a b
  (.|.) (Literal a) (Literal b) = Literal (a .|. b)
  (.|.) a b = BinOp BitwiseOr  a b
  xor (Literal a) (Literal b) = Literal (a `xor` b) 
  xor   a b = BinOp BitwiseXor a b 
  
  --TODO: See that this is not breaking something (32/64 bit, CUDA/Haskell)
  complement (Literal i) = Literal (complement i)
  
  complement a = UnOp BitwiseNeg a
  shiftL a i = BinOp ShiftL  a (Literal i)
  shiftR a i = BinOp ShiftR  a (Literal i)
  bitSize a  = sizeOf a * 8
  isSigned a = True

----------------------------------------------------------------------------
-- Word32 Instances 
instance Num (Exp Word32) where 
  (+) a (Literal 0) = a
  (+) (Literal 0) a = a
  (+) (Literal a) (Literal b) = Literal (a+b)
  (+) a b = BinOp Add a b  
  
  (-) a (Literal 0) = a 
  (-) (Literal a) (Literal b) = Literal (a - b) 
  (-) a b = BinOp Sub a b 
  
  (*) a (Literal 0) = 0
  (*) (Literal 0) a = 0 
  (*) a (Literal 1) = a 
  (*) (Literal 1) a = a
  (*) a b = BinOp Mul a b 
  
  signum = undefined 
  abs = undefined
  fromInteger a = Literal (fromInteger a) 
  
 -- Added new cases for literal 0 (2012/09/25)
instance Bits (Exp Word32) where 
  (.&.) x (Literal 0) = Literal 0
  (.&.) (Literal 0) x = Literal 0 
  (.&.) (Literal a) (Literal b) = Literal (a .&. b) 
  (.&.) a b = BinOp BitwiseAnd a b   
  (.|.) (Literal a) (Literal b) = Literal (a .|. b) 
  (.|.) a b = BinOp BitwiseOr  a b
  xor (Literal a) (Literal b) = Literal (a `xor` b) 
  xor   a b = BinOp BitwiseXor a b 
  complement (Literal i) = Literal (complement i) 
  complement a = UnOp BitwiseNeg a
  
  shiftL (Literal j) i = Literal (j `shiftL` i) 
  shiftL a i = BinOp ShiftL a (Literal i)
  
  shiftR (Literal j) i = Literal (j `shiftL` i)
  shiftR a i = BinOp ShiftR a (Literal i)
  bitSize a  = 32
  isSigned a = False

instance Real (Exp Word32) where 
  toRational = undefined
  

instance Enum (Exp Word32) where
  toEnum = undefined
  fromEnum = undefined

instance Integral (Exp Word32) where
  mod a b = BinOp Mod a b 
  div a b = BinOp Div a b
  quotRem = undefined
  toInteger = undefined
  
instance Num (Exp Float) where
  (+) a (Literal 0) = a
  (+) (Literal 0) a = a
  (+) (Literal a) (Literal b) = Literal (a + b)
  (+) a b = BinOp Add a b
  
  (-) a (Literal 0) = a
  (-) (Literal a) (Literal b) = Literal (a - b)
  (-) a b = BinOp Sub a b
  
  (*) a (Literal 1) = a
  (*) (Literal 1) a = a
  (*) _ (Literal 0) = Literal 0
  (*) (Literal 0) _ = Literal 0
  (*) (Literal a) (Literal b) = Literal (a * b)
  (*) a b = BinOp Mul a b
  
  signum = undefined
  abs = undefined
  fromInteger a = Literal (fromInteger a)

instance Fractional (Exp Float) where
  (/) a b = BinOp FDiv a b
  recip a = (Literal 1) / a
  fromRational a = Literal (fromRational a)

instance Floating (Exp Float) where
  pi = Literal pi
  exp a = UnOp Exp a
  sqrt a = UnOp Sqrt a
  log a = UnOp Log a
  (**) a b = BinOp Pow a b
  
  -- log_b(x) = log_e(x) / log_e(b)
  logBase (Literal 2) b = UnOp Log2 b
  logBase (Literal 10) b = UnOp Log10 b
  logBase a b = (UnOp Log b) / (UnOp Log a)
  
  sin (Literal 0) = Literal 0
  sin a = UnOp Sin a
  tan (Literal 0) = Literal 0
  tan a = UnOp Tan a
  cos (Literal 0) = Literal 1
  cos a = UnOp Cos a
  
  asin (Literal 0) = Literal 0
  asin a = UnOp ASin a
  atan (Literal 0) = Literal 0
  atan a = UnOp ATan a
  acos (Literal 1) = Literal 0
  acos a = UnOp ACos a
  
  sinh (Literal 0) = Literal 0
  sinh a = UnOp Sin a
  tanh (Literal 0) = Literal 0
  tanh a = UnOp Tan a
  cosh (Literal 0) = Literal 1
  cosh a = UnOp Cos a
  
  asinh a = UnOp ASinH a
  atanh a = UnOp ATanH a
  acosh a = UnOp ACosH a
  
  -- Y-Less's comment
  -- Don't second guess the CUDA compiler (or, more accurately, assume that
  -- other compilers have this).
  --(/) (Literal 1) (UnOp Sqrt b) = UnOp RSqrt b -- Optimisation.

  
  
  
----------------------------------------------------------------------------
  
infix 4 ==*, /=*, <*, >*, >=*, <=* 
  
(==*) (Literal a) (Literal b) = Literal (a == b) 
(==*) a b = BinOp Eq a b
(/=*) a b = BinOp NotEq a b 
(<*)  (Literal a) (Literal b) = Literal (a < b) 
(<*)  a b = BinOp Lt a b
(<=*) (Literal a) (Literal b) = Literal (a <= b) 
(<=*) a b = BinOp LEq a b
(>*)  a b = BinOp Gt  a b
(>=*) a b = BinOp GEq a b

infixr 3 &&*
infixr 2 ||* 
(&&*) a b = BinOp And a b 
(||*) a b = BinOp Or a b 

class Choice a where 
  ifThenElse :: Exp Bool -> a -> a -> a 

instance Scalar a => Choice (Exp a) where  
  ifThenElse (Literal False) e1 e2 = e2
  ifThenElse (Literal True)  e1 e2 = e1
  ifThenElse b e1 e2 = If b e1 e2
  
instance (Choice a, Choice b) => Choice (a,b) where
  ifThenElse b (e1,e1') (e2,e2') = (ifThenElse b e1 e2,
                                    ifThenElse b e1' e2') 
  

----------------------------------------------------------------------------
-- Built-ins


----------------------------------------------------------------------------
-- Print Expressions

printExp :: Scalar a => Exp a -> String
printExp (Literal a) = show a 
printExp (Index (name,[])) = name
printExp (Index (name,es)) = 
  name ++ "[" ++ ((concat . intersperse "," . map printExp) es) ++ "]"
printExp (IndexGlobal name bsize bix tix) = 
  name ++ "[" ++ printExp bix ++ "*" ++ 
                 printExp bsize ++ "+" ++ 
                 printExp tix ++ "]"

printExp (BinOp op e1 e2) = "(" ++ printOp op ++ " " ++  printExp e1 ++ " " ++ printExp e2 ++ " )"
printExp (UnOp  op e) = "(" ++ printOp op ++ " " ++ printExp e ++ " )"
printExp (If b e1 e2) = "(" ++ printExp b ++ " ? " ++ printExp e1 ++ " : " ++ printExp e2 ++ ")"
printExp ThreadIdx = "threadIdx.x"
printExp BlockIdx  = "blockIdx.x" 

printOp :: Op a -> String
printOp Add = " + " 
printOp Sub = " - " 
printOp Mul = " * "

-- printOp If  = " if "

printOp Eq  = " == "
printOp NotEq = " /= " 
printOp Lt  = " < " 
printOp LEq = " <= " 
printOp Gt  = " > "
printOp GEq = " >= " 

printOp And = " && "
printOp Or  = " || " 

printOp Min = " Min "
printOp Max = " Max " 

printOp Sin = " Sin " 
printOp Cos = " Cos "

printOp BitwiseAnd = " & "
printOp BitwiseOr  = " | " 
printOp BitwiseXor = " ^ " 
printOp BitwiseNeg = " ~ "  



---------------------------------------------------------------------------- 
-- Experimenting 

class ExpToCExp a where 
  expToCExp :: Exp a -> CExpr 


instance  ExpToCExp Bool where 
  expToCExp (Literal True) = cLiteral (IntVal 1) CInt 
  expToCExp (Literal False) = cLiteral (IntVal 0) CInt
  expToCExp a = expToCExpGeneral a 

instance ExpToCExp Int where 
  expToCExp (Literal a) = cLiteral (IntVal a) CInt
  expToCExp a = expToCExpGeneral a  

instance ExpToCExp Float where 
  expToCExp (Literal a) = cLiteral (FloatVal a) CFloat
  expToCExp a = expToCExpGeneral a 

instance ExpToCExp Double where 
  expToCExp (Literal a) = cLiteral (DoubleVal a) CDouble
  expToCExp a = expToCExpGeneral a 

instance ExpToCExp Word8 where 
  expToCExp (Literal a) = cLiteral (Word8Val a) CWord8
  expToCExp a = expToCExpGeneral a 

instance ExpToCExp Word16 where 
  expToCExp (Literal a) = cLiteral (Word16Val a) CWord16
  expToCExp a = expToCExpGeneral a 

instance ExpToCExp Word32 where 
  expToCExp (Literal a) = cLiteral (Word32Val a) CWord32
  expToCExp a = expToCExpGeneral a 

instance ExpToCExp Word64 where 
  expToCExp (Literal a) = cLiteral (Word64Val a) CWord64
  expToCExp a = expToCExpGeneral a 

  
expToCExpGeneral :: ExpToCExp a  => Exp a -> CExpr 
expToCExpGeneral (BlockIdx) = cBlockIdx X 
expToCExpGeneral (ThreadIdx) = cThreadIdx X
--expToCExpGeneral (BlockDim d) = cBlockDim d
--expToCExpGeneral (GridDim d)  = cGridDim d

expToCExpGeneral e@(Index (name,[])) = cVar name (typeToCType (typeOf e))
expToCExpGeneral e@(Index (name,xs)) = cIndex (cVar name (CPointer (typeToCType (typeOf e))),map expToCExp xs) (typeToCType (typeOf e)) 
expToCExpGeneral e@(If b e1 e2)      = cCond  (expToCExp b) (expToCExp e1) (expToCExp e2) (typeToCType (typeOf e)) 
expToCExpGeneral e@(BinOp Min e1 e2) = cFuncExpr "min" [expToCExp e1, expToCExp e2] (typeToCType (typeOf e)) 
expToCExpGeneral e@(BinOp Max e1 e2) = cFuncExpr "max" [expToCExp e1, expToCExp e2] (typeToCType (typeOf e)) 
expToCExpGeneral e@(BinOp op e1 e2)  = cBinOp (binOpToCBinOp op) (expToCExp e1) (expToCExp e2) (typeToCType (typeOf e)) 

expToCExpGeneral (UnOp Exp e)        = cFuncExpr "exp" [expToCExp e] (typeToCType (typeOf e))
expToCExpGeneral (UnOp Sqrt e)       = cFuncExpr "sqrt" [expToCExp e] (typeToCType (typeOf e))
expToCExpGeneral (UnOp Log e)        = cFuncExpr "log" [expToCExp e] (typeToCType (typeOf e))
expToCExpGeneral (UnOp Log2 e)       = cFuncExpr "log2" [expToCExp e] (typeToCType (typeOf e))
expToCExpGeneral (UnOp Log10 e)      = cFuncExpr "log10" [expToCExp e] (typeToCType (typeOf e))
  
-- Floating trig
expToCExpGeneral (UnOp Sin e)        = cFuncExpr "sin" [expToCExp e] (typeToCType (typeOf e))
expToCExpGeneral (UnOp Cos e)        = cFuncExpr "cos" [expToCExp e] (typeToCType (typeOf e))
expToCExpGeneral (UnOp Tan e)        = cFuncExpr "tan" [expToCExp e] (typeToCType (typeOf e))
expToCExpGeneral (UnOp ASin e)       = cFuncExpr "asin" [expToCExp e] (typeToCType (typeOf e))
expToCExpGeneral (UnOp ACos e)       = cFuncExpr "acos" [expToCExp e] (typeToCType (typeOf e))
expToCExpGeneral (UnOp ATan e)       = cFuncExpr "atan" [expToCExp e] (typeToCType (typeOf e))
expToCExpGeneral (UnOp SinH e)       = cFuncExpr "sinh" [expToCExp e] (typeToCType (typeOf e))
expToCExpGeneral (UnOp CosH e)       = cFuncExpr "cosh" [expToCExp e] (typeToCType (typeOf e))
expToCExpGeneral (UnOp TanH e)       = cFuncExpr "tanh" [expToCExp e] (typeToCType (typeOf e))
expToCExpGeneral (UnOp ASinH e)      = cFuncExpr "asinh" [expToCExp e] (typeToCType (typeOf e))
expToCExpGeneral (UnOp ACosH e)      = cFuncExpr "acosh" [expToCExp e] (typeToCType (typeOf e))
expToCExpGeneral (UnOp ATanH e)      = cFuncExpr "atanh" [expToCExp e] (typeToCType (typeOf e))
 
expToCExpGeneral e@(UnOp  op e1)     = cUnOp  (unOpToCUnOp op) (expToCExp e1) (typeToCType (typeOf e)) 

typeToCType Bool = CInt 
typeToCType Int  = CInt
typeToCType Float = CFloat
typeToCType Double = CDouble
typeToCType Word8 = CWord8
typeToCType Word16 = CWord16
typeToCType Word32 = CWord32
typeToCType Word64 = CWord64
typeToCType (Pointer t) = CPointer (typeToCType t)
typeToCType (Global t)  = CQualified CQualifyerGlobal (typeToCType t) 
typeToCType (Local t)  = CQualified CQualifyerLocal (typeToCType t) 

-- maybe unnecessary
binOpToCBinOp Add = CAdd
binOpToCBinOp Sub = CSub
binOpToCBinOp Mul = CMul
binOpToCBinOp Div = CDiv 
binOpToCBinOp FDiv = CDiv -- (???)  
binOpToCBinOp Mod = CMod

binOpToCBinOp Eq  = CEq 
binOpToCBinOp NotEq = CNotEq
binOpToCBinOp Lt  = CLt 
binOpToCBinOp LEq = CLEq
binOpToCBinOp Gt  = CGt 
binOpToCBinOp GEq = CGEq 

binOpToCBinOp And = CAnd 
binOpToCBinOp Or  = COr 

binOpToCBinOp Pow = CPow

binOpToCBinOp BitwiseAnd = CBitwiseAnd
binOpToCBinOp BitwiseOr  = CBitwiseOr
binOpToCBinOp BitwiseXor = CBitwiseXor
binOpToCBinOp ShiftL     = CShiftL 
binOpToCBinOp ShiftR     = CShiftR
-- notice min and max is not here ! 

unOpToCUnOp   BitwiseNeg = CBitwiseNeg
  

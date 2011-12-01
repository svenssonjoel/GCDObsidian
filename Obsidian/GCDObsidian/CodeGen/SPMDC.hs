module Obsidian.GCDObsidian.CodeGen.SPMDC where


import Obsidian.GCDObsidian.Globs

import Data.Word
import qualified Data.List as List
import qualified Data.Map as Map

import Control.Monad.State

----------------------------------------------------------------------------
-- 
data Value = IntVal Int
           | FloatVal Float 
           | DoubleVal Double
           | Word8Val  Word8
           | Word16Val Word16
           | Word32Val Word32
           | Word64Val Word64
           deriving (Eq,Ord,Show)
             
data CType = CVoid | CInt | CFloat | CDouble              
           | CWord8 | CWord16 | CWord32 | CWord64
           | CPointer CType 
           | CQualified CQualifyer CType 
           deriving (Eq,Ord,Show)
             
data CQualifyer = CQualifyerGlobal 
                | CQualifyerLocal
                | CQualifyerKernel
                deriving (Eq,Ord,Show)


data CExprP e  = CVar Name
               | CLiteral Value 
               | CIndex (e,[e]) 
               | CCond e e e
               | CBinOp CBinOp e e
               | CUnOp  CUnOp  e 
               | CFuncExpr Name [e]   -- min, max, sin, cos 
               | CCast CType e        -- cast expr to type 
               deriving (Eq,Ord,Show)
                        
data CBinOp = CAdd | CSub | CMul | CDiv | CMod  
            | CEq | CLt | CLEq | CGt | CGEq 
            | CBitwiseAnd | CBitwiseOr | CBitwiseXor 
            | CShiftL | CShiftR 
            deriving (Eq,Ord,Show) 
                     
data CUnOp = CBitwiseNeg                     
           deriving (Eq,Ord,Show)


{-
   SPMDC and CKernel may turn more complicated if we 
   add features. 
    - loops is an example.. 
       + Already in normal C code generation this will be an issue. 
       
-} 
--                  targ   ifarr  source
--
data SPMDC = CAssign Name [CExpr] CExpr -- array or scalar assign 
           -- Target should be just a name, for less chance of confusio
           | CDeclAssign CType Name CExpr -- declare variable and assign a value 
           | CFunc   Name  [CExpr]       
           | CSync                  -- CUDA and OpenCL
           | CIf     CExpr [SPMDC] [SPMDC]
           deriving (Eq,Ord,Show)
                    
--                                ret_t          inputs       body
data CKernel = CKernel CQualifyer CType Name [(CType,Name)] [SPMDC] 
             deriving (Eq,Show)
           
----------------------------------------------------------------------------
-- CExpr 
newtype CExpr = CExpr (CExprP CExpr)
             deriving (Eq,Ord,Show)
                      
--newtype SPMDC = S (SPMDCP CExpr) 
--              deriving (Eq,Ord,Show)

----------------------------------------------------------------------------                      
-- DAGs
type NodeID = Integer                
newtype CENode = CENode (CExprP NodeID) 
               deriving Show
--newtype SNode = SNode (SPMDCP NodeID) 
--              deriving Show
----------------------------------------------------------------------------
-- 
cVar n            = CExpr $ CVar n
cLiteral l        = CExpr $ CLiteral l
cIndex a          = CExpr $ CIndex a
cCond b e1 e2     = CExpr $ CCond b e1 e2
cFuncExpr n es    = CExpr $ CFuncExpr n es
cBinOp op e1 e2   = CExpr $ CBinOp op e1 e2
cUnOp op e        = CExpr $ CUnOp op e 
cCast t e         = CExpr $ CCast t e  

cAssign = CAssign 
cFunc = CFunc  
cDeclAssign = CDeclAssign 
cIf = CIf 

--------------------------------------------------------------------------
-- Printing 

-- TODO: Rewrite the printing to have indentation etc. 

printCKernel :: CKernel -> String 
printCKernel (CKernel q t nom ins body) = 
  printCQual q ++ " " ++ printCType t ++ " " ++ nom ++ commaSepList pins "(" ")" ins ++ 
  "{\n" ++
  printBody body ++ 
  "}\n"
  where 
    pins (t,nom) = printCType t ++ " " ++ nom
  

commaSepList printElt s e xs = s ++ concat (List.intersperse "," (commaSepList' xs)) ++ e
  where 
    commaSepList' [] = [] 
    commaSepList' (x:xs) = printElt x : commaSepList' xs
  
printCQual CQualifyerGlobal  = "__global__"
printCQual CQualifyerLocal   = "" -- "__local"
printCQual CQualifyerKernel  = "__global__" -- "__kernel"

printCType CVoid    = "void"
printCType CInt     = "int"
printCType CFloat   = "float"
printCType CDouble  = "double"            
printCType CWord8   = "uint8_t"
printCType CWord16  = "uint16_t"
printCType CWord32  = "uint32_t"
printCType CWord64  = "uint64_t" 
printCType (CPointer t) = printCType t ++ "*"
printCType (CQualified q t) = printCQual q ++ " " ++ printCType t


printBody [] = "" 
printBody (x:xs) = printSPMDC x ++ printBody xs

printSPMDC (CAssign nom [] expr) = nom  ++ " = " ++ printCExpr expr  ++ ";\n" 
printSPMDC (CAssign nom exprs expr) = nom  ++ commaSepList printCExpr "[" "]" exprs ++ " = " ++ printCExpr expr ++ ";\n" 
printSPMDC (CDeclAssign t n e) = printCType t ++ " " ++ n ++ " = " ++ printCExpr e ++ ";\n" 
printSPMDC (CFunc nom args) = nom ++ commaSepList printCExpr "(" ")" args ++ ";\n"
printSPMDC CSync  = "__syncthreads();\n"
printSPMDC (CIf e [] [] ) = "" -- 
printSPMDC (CIf e xs [] ) =  "if " ++ printCExpr e ++ "{\n" ++ concatMap printSPMDC xs ++ "}\n"
printSPMDC (CIf e xs ys ) =  "if " ++ printCExpr e ++ "{\n" ++ concatMap printSPMDC xs ++ "}\n" ++ 
                                 "else {\n" ++ concatMap printSPMDC ys ++ "}\n"


printCExpr :: CExpr -> String 
printCExpr (CExpr (CVar nom)) = nom
printCExpr (CExpr (CLiteral v)) = printValue v 
printCExpr (CExpr (CIndex (e,[]))) = printCExpr e 
printCExpr (CExpr (CIndex (e,xs))) = printCExpr e  ++ commaSepList printCExpr "[" "]" xs
printCExpr (CExpr (CCond e1 e2 e3))    = printCExpr e1 ++ " ? " ++ printCExpr e2 ++ " : " ++ printCExpr e3
printCExpr (CExpr (CBinOp bop e1 e2)) = "(" ++ printCExpr e1 ++ printBinOp bop ++ printCExpr e2 ++ ")"
printCExpr (CExpr (CUnOp  uop  e)) = "(" ++ printUnOp uop ++ printCExpr e ++ ")" 
printCExpr (CExpr (CFuncExpr nom args)) = nom ++ commaSepList printCExpr "(" ")" args
printCExpr (CExpr (CCast t e)) = "((" ++ printCType t ++ ")" ++ printCExpr e ++ ")"

printValue (IntVal i) = show i
printValue (FloatVal f) = show f 
printValue (DoubleVal d) = show d
printValue (Word8Val  w) = show w 
printValue (Word16Val w) = show w
printValue (Word32Val w) = show w
printValue (Word64Val w) = show w 


printBinOp CAdd = "+"
printBinOp CSub = "-"
printBinOp CMul = "*"
printBinOp CDiv = "/"
printBinOp CMod = "%" 
printBinOp CEq  = "=="
printBinOp CLt  = "<" 
printBinOp CLEq = "<="
printBinOp CGt  = ">" 
printBinOp CGEq = ">="
printBinOp CBitwiseAnd = "&"  
printBinOp CBitwiseOr  = "|" 
printBinOp CBitwiseXor = "^" 
printBinOp CShiftL     = "<<" 
printBinOp CShiftR     = ">>"
                     
printUnOp CBitwiseNeg = "~"       


----------------------------------------------------------------------------
-- a small test.

cprg1 = CKernel CQualifyerKernel CVoid "apa" [(CInt,"a"),(CFloat, "b")] 
        [ cAssign "apa" [cLiteral (IntVal 5)] (cLiteral (IntVal 5))
        , cFunc "__syncthreads" []
        ]

spmdcTest1 =  putStrLn$ printCKernel cprg1

---------------------------------------------------------------------------- 
-- CExpr to Dag and back again. 

type CSEMap = Map.Map CExpr (NodeID,CENode)

newNodeID = do 
  i <- get 
  put (i+1)
  return i

insertCM :: CSEMap -> CExpr -> CENode -> State NodeID (CSEMap,NodeID) 
insertCM cm expr node = 
  case Map.lookup expr cm of 
    (Just (i,n)) -> return (cm, i)
    Nothing  -> do
      i <- newNodeID 
      let cm' = Map.insert expr (i,node) cm 
      return (cm',i)
  

cExprToDag :: CSEMap -> CExpr -> State NodeID (CSEMap,NodeID) 
cExprToDag cm e@(CExpr (CVar nom)) = do
  insertCM cm e (CENode (CVar nom))
cExprToDag cm e@(CExpr (CLiteral l)) = do 
  insertCM cm e (CENode (CLiteral l))
  
cExprToDag cm exp@(CExpr (CIndex (e,es))) = do 
  (cm1,e') <- cExprToDag cm e
  (cm2,es') <- cExprListToDag cm1 es 
  insertCM cm2 exp (CENode (CIndex (e',es')))

cExprToDag cm exp@(CExpr (CBinOp op e1 e2)) = do   
  (cm1,i1) <- cExprToDag cm e1
  (cm2,i2) <- cExprToDag cm1 e2 
  insertCM cm2 exp (CENode (CBinOp op i1 i2))
cExprToDag cm exp@(CExpr (CUnOp op e)) = do    
  (cm1,i1) <- cExprToDag cm e
  insertCM cm1 exp (CENode (CUnOp op i1))

  
cExprListToDag :: CSEMap -> [CExpr]  -> State NodeID (CSEMap,[NodeID])                  
cExprListToDag cm [] = return (cm,[])
cExprListToDag cm (x:xs) = do 
  (cm', xs') <- cExprListToDag cm xs 
  (cmEnd,x') <- cExprToDag cm' x 
  return (cmEnd, x':xs')

--spmdcToDag :: CSEMap -> [SPMDC] -> State NodeID (CSEMap,[SNode])
--spmdcToDag cm sp = undefined

performCSE :: CSEMap -> SPMDC -> SPMDC 
performCSE = undefined

--dagToSpmdc :: CSEMap -> [SNode] -> [SPMDC] 
--dagToSpmdc cm sn = undefined 

-- dagToCExpr :: CSEMap -> CENode -> CExpr
-- dagToCExpr cm expr = undefined 





----------------------------------------------------------------------------
-- Examples

small = CExpr$ CBinOp CAdd (CExpr (CVar "a")) (CExpr (CVar "b"))
large = CExpr$ CBinOp CAdd small small 
huge  = CExpr$ CBinOp CAdd large large
testExpr = CExpr (CVar "hej")
toDag1 = Map.elems$ fst$ fst$ runState (cExprToDag Map.empty huge) 0
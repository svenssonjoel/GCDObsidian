module Obsidian.GCDObsidian.CodeGen.SPMDC where


import Obsidian.GCDObsidian.Globs

import Data.Word
import qualified Data.List as List



----------------------------------------------------------------------------
-- 
data Value = IntVal Int
           | FloatVal Float 
           | DoubleVal Double
           | Word8Val  Word8
           | Word16Val Word16
           | Word32Val Word32
           | Word64Val Word64
           deriving (Eq,Show)
             
data CType = CVoid | CInt | CFloat | CDouble              
           | CWord8 | CWord16 | CWord32 | CWord64
           | CPointer CType 
           | CQualified CQualifyer CType 
           deriving (Eq,Show)
             
data CQualifyer = CQualifyerGlobal 
                | CQualifyerLocal
                | CQualifyerKernel
                deriving (Eq,Show)


data CExprP e  = CVar Name
               | CLiteral Value 
               | CIndex (e,[e]) 
               | CCond e e e
               | CBinOp CBinOp e e
               | CUnOp  CUnOp  e 
               | CFuncExpr Name [e]   -- min, max, sin, cos 
               | CCast CType e        -- cast expr to type 
               deriving (Eq,Show)
                        
data CBinOp = CAdd | CSub | CMul | CDiv | CMod  
            | CEq | CLt | CLEq | CGt | CGEq 
            | CBitwiseAnd | CBitwiseOr | CBitwiseXor 
            | CShiftL | CShiftR 
            deriving (Eq,Show) 
                     
data CUnOp = CBitwiseNeg                     
           deriving (Eq,Show)


{-
   SPMDC and CKernel may turn more complicated if we 
   add features. 
    - loops is an example.. 
       + Already in normal C code generation this will be an issue. 
       
-} 
--                   targ   ifarr  source
data SPMDCP e a = CAssign e [e] e -- array or scalar assign 
           | CDeclAssign CType Name e -- declare variable and assign a value 
           | CFunc   Name  [e]       
           | CSync                  -- CUDA and OpenCL
           | CIf     e [a] [a]
           deriving (Eq,Show)

--                                ret_t          inputs       body
data CKernel = CKernel CQualifyer CType Name [(CType,Name)] [SPMDC] 
             deriving (Eq,Show)
           
----------------------------------------------------------------------------
-- CExpr 
newtype CExpr = CExpr (CExprP CExpr)
             deriving (Eq,Show)
                      
newtype SPMDC = S (SPMDCP CExpr SPMDC) 
              deriving (Eq,Show)

----------------------------------------------------------------------------                      
-- DAGs
type NodeID = Integer                
newtype CENode = CENode (CExprP NodeID) 
newtype SNode = SNode (SPMDCP NodeID SNode) 

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

cAssign e1 es e2 = S $ CAssign e1 es e2
cFunc   n args = S $ CFunc n args 
cDeclAssign t n e = S $ CDeclAssign t n e 
cIf b s1 s2 = S $ CIf b s1 s2 

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

printSPMDC (S (CAssign e [] expr)) = printCExpr e  ++ " = " ++ printCExpr expr  ++ ";\n" 
printSPMDC (S (CAssign e exprs expr)) = printCExpr e  ++ commaSepList printCExpr "[" "]" exprs ++ " = " ++ printCExpr expr ++ ";\n" 
printSPMDC (S (CDeclAssign t n e)) = printCType t ++ " " ++ n ++ " = " ++ printCExpr e ++ ";\n" 
printSPMDC (S (CFunc nom args)) = nom ++ commaSepList printCExpr "(" ")" args ++ ";\n"
printSPMDC (S CSync)  = "__syncthreads();\n"
printSPMDC (S (CIf e [] [] )) = "" -- 
printSPMDC (S (CIf e xs [] )) =  "if " ++ printCExpr e ++ "{\n" ++ concatMap printSPMDC xs ++ "}\n"
printSPMDC (S (CIf e xs ys )) =  "if " ++ printCExpr e ++ "{\n" ++ concatMap printSPMDC xs ++ "}\n" ++ 
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
        [ cAssign (cVar "apa") [cLiteral (IntVal 5)] (cLiteral (IntVal 5))
        , cFunc "__syncthreads" []
        ]

spmdcTest1 =  putStrLn$ printCKernel cprg1

---------------------------------------------------------------------------- 
-- CExpr to Dag

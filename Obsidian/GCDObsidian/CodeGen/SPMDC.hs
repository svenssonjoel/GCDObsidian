module Obsidian.GCDObsidian.CodeGen.SPMDC where


import Obsidian.GCDObsidian.Globs

import Data.Word
import qualified Data.List as List

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


data CExpr = CVar Name
           | CLiteral Value 
           | CIndex (CExpr,[CExpr]) 
           | CCond CExpr CExpr CExpr
           | CBinOp CBinOp CExpr CExpr
           | CUnOp  CUnOp  CExpr 
           | CFuncExpr Name [CExpr]   -- min, max, sin, cos 
           | CCast CType CExpr        -- cast expr to type 
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
data SPMDC = CAssign CExpr [CExpr] CExpr -- array or scalar assign 
           | CFunc   Name  [CExpr]       
           | CSync                  -- CUDA and OpenCL
           | CIf     CExpr [SPMDC] [SPMDC]
           deriving (Eq,Show)



--                                ret_t          inputs       body
data CKernel = CKernel CQualifyer CType Name [(CType,Name)] [SPMDC] 
             deriving (Eq,Show)
           


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

printSPMDC (CAssign e [] expr) = printCExpr e  ++ " = " ++ printCExpr expr  ++ ";\n" 
printSPMDC (CAssign e exprs expr) = printCExpr e  ++ commaSepList printCExpr "[" "]" exprs ++ " = " ++ printCExpr expr ++ ";\n" 
printSPMDC (CFunc nom args) = nom ++ commaSepList printCExpr "(" ")" args ++ ";\n"
printSPMDC CSync = "__syncthreads();\n"
printSPMDC (CIf e [] [] ) = "" -- 
printSPMDC (CIf e xs [] ) =  "if " ++ printCExpr e ++ "{\n" ++ concatMap printSPMDC xs ++ "}\n"
printSPMDC (CIf e xs ys ) =  "if " ++ printCExpr e ++ "{\n" ++ concatMap printSPMDC xs ++ "}\n" ++ 
                             "else {\n" ++ concatMap printSPMDC ys ++ "}\n"



printCExpr (CVar nom) = nom
printCExpr (CLiteral v) = printValue v 
printCExpr (CIndex (e,[])) = printCExpr e 
printCExpr (CIndex (e,xs)) = printCExpr e  ++ commaSepList printCExpr "[" "]" xs
printCExpr (CCond e1 e2 e3)    = printCExpr e1 ++ " ? " ++ printCExpr e2 ++ " : " ++ printCExpr e3
printCExpr (CBinOp bop e1 e2) = "(" ++ printCExpr e1 ++ printBinOp bop ++ printCExpr e2 ++ ")"
printCExpr (CUnOp  uop  e) = "(" ++ printUnOp uop ++ printCExpr e ++ ")" 
printCExpr (CFuncExpr nom args) = nom ++ commaSepList printCExpr "(" ")" args
printCExpr (CCast t e) = "((" ++ printCType t ++ ")" ++ printCExpr e ++ ")"

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




cprg1 = CKernel CQualifyerKernel CVoid "apa" [(CInt,"a"),(CFloat, "b")] 
        [ CAssign (CVar "apa") [CLiteral (IntVal 5)] (CLiteral (IntVal 5))
        , CFunc "__syncthreads" []
        ]
        
spmdcTest1 =  putStrLn$ printCKernel cprg1
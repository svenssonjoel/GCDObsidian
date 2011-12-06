module Obsidian.GCDObsidian.CodeGen.SPMDC where


import Obsidian.GCDObsidian.Globs

import Obsidian.GCDObsidian.CodeGen.PP

import Data.Word
import qualified Data.List as List
import qualified Data.Map as Map

import Control.Monad.State

import Data.Maybe

-- A C LIKE AST (SPMDC - Single Program Multiple Data C) 


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


data CExprP e  = CVar Name CType 
               | CLiteral Value CType
               | CIndex (e,[e]) CType
               | CCond e e e CType
               | CBinOp CBinOp e e  CType
               | CUnOp  CUnOp  e    CType
               | CFuncExpr Name [e] CType  -- min, max, sin, cos 
               | CCast e CType            -- cast expr to type 
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
data SPMDC = CAssign CExpr [CExpr] CExpr -- array or scalar assign 
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
                      
----------------------------------------------------------------------------                      
-- DAGs
type NodeID = Integer                
newtype CENode = CENode (CExprP NodeID) 
               deriving Show
                        
----------------------------------------------------------------------------
-- Helpers 

cexpr1 exp a       = CExpr $ exp a 
cexpr2 exp a b     = CExpr $ exp a b 
cexpr3 exp a b c   = CExpr $ exp a b c 
cexpr4 exp a b c d = CExpr $ exp a b c d  

cVar      = cexpr2 CVar 
cLiteral  = cexpr2 CLiteral 
cIndex    = cexpr2 CIndex 
cCond     = cexpr4 CCond   
cFuncExpr = cexpr3 CFuncExpr 
cBinOp    = cexpr4 CBinOp 
cUnOp     = cexpr3 CUnOp 
cCast     = cexpr2 CCast 

cAssign = CAssign 
cFunc = CFunc  
cDeclAssign = CDeclAssign 
cIf = CIf 

--------------------------------------------------------------------------
-- Printing 

{- 
  TODO: 
     + Rewrite the printing to have indentation etc. 
         Maybe reuse the stuff from CodeGen.Common
     + enable "Print as CUDA" and "Print as OpenCL" 
-} 

data PPConfig = PPConfig {ppKernelQ :: String, 
                          ppGlobalQ :: String, 
                          ppLocalQ  :: String,
                          ppSyncLine :: String} 

printCKernel :: PPConfig -> CKernel -> String 
printCKernel ppc kern = runPP (ppCKernel ppc  kern ) 0 

ppCKernel :: PPConfig -> CKernel -> PP () 
ppCKernel ppc (CKernel q t nom ins body) = 
  ppCQual ppc q >> space >> ppCType ppc t >> space >> line nom >> ppCommaSepList ppIns "(" ")" ins >> 
  begin >> indent >> newline >> 
  ppSPMDCList ppc body >>  unindent >> newline >>
  end 
  where 
    ppIns (t,nom) = ppCType ppc t >> space >> line nom
  
----------------------------------------------------------------------------
ppCQual ppc CQualifyerGlobal = line$ ppGlobalQ ppc 
ppCQual ppc CQualifyerLocal  = line$ ppLocalQ ppc 
ppCQual ppc CQualifyerKernel = line$ ppKernelQ ppc 

space = line " " 

----------------------------------------------------------------------------
ppCType ppc CVoid    = line "void"
ppCType ppc CInt     = line "int"
ppCType ppc CFloat   = line "float"
ppCType ppc CDouble  = line "double"            
ppCType ppc CWord8   = line "uint8_t"
ppCType ppc CWord16  = line "uint16_t"
ppCType ppc CWord32  = line "uint32_t"
ppCType ppc CWord64  = line "uint64_t" 
ppCType ppc (CPointer t) = ppCType ppc t >> line "*"
ppCType ppc (CQualified q t) = ppCQual ppc q >> space >> ppCType ppc t

----------------------------------------------------------------------------
ppValue (IntVal i) = line$ show i
ppValue (FloatVal f) = line$ show f 
ppValue (DoubleVal d) = line$ show d
ppValue (Word8Val  w) = line$ show w 
ppValue (Word16Val w) = line$ show w
ppValue (Word32Val w) = line$ show w
ppValue (Word64Val w) = line$ show w 

----------------------------------------------------------------------------
ppBinOp CAdd = line$ "+"
ppBinOp CSub = line$ "-"
ppBinOp CMul = line$ "*"
ppBinOp CDiv = line$ "/"
ppBinOp CMod = line$ "%" 
ppBinOp CEq  = line$ "=="
ppBinOp CLt  = line$ "<" 
ppBinOp CLEq = line$ "<="
ppBinOp CGt  = line$ ">" 
ppBinOp CGEq = line$ ">="
ppBinOp CBitwiseAnd = line$ "&"  
ppBinOp CBitwiseOr  = line$ "|" 
ppBinOp CBitwiseXor = line$ "^" 
ppBinOp CShiftL     = line$ "<<" 
ppBinOp CShiftR     = line$ ">>"
                     
ppUnOp CBitwiseNeg = line$ "~"       


----------------------------------------------------------------------------
--
ppCommaSepList ppElt s e xs = 
  line s >>  
  sequence_ (List.intersperse (line ",") (commaSepList' xs)) >> line e
  where 
    commaSepList' [] = [] 
    commaSepList' (x:xs) = ppElt x : commaSepList' xs
  
----------------------------------------------------------------------------
--
ppSPMDCList ppc xs = sequence_ (map (ppSPMDC ppc) xs) 


ppSPMDC :: PPConfig -> SPMDC -> PP () 
ppSPMDC ppc (CAssign e [] expr) = ppCExpr ppc e >> line " = " >> ppCExpr ppc expr >> line ";" >> newline
ppSPMDC ppc (CAssign e exprs expr) = ppCExpr ppc e >> 
                                     ppCommaSepList (ppCExpr ppc) "[" "]" exprs >> 
                                     line " = " >> 
                                     ppCExpr ppc expr >> 
                                     line ";" >> newline
ppSPMDC ppc (CDeclAssign t n e) = ppCType ppc t >> space >> line n >> line " = " >> ppCExpr ppc e >> line ";" >> newline
ppSPMDC ppc (CFunc nom args) = line nom >> ppCommaSepList (ppCExpr ppc) "(" ")" args >> line ";" >> newline 
ppSPMDC ppc  CSync = line$ ppSyncLine ppc
ppSPMDC ppc (CIf e [] []) = return ()
ppSPMDC ppc (CIf e xs []) = line "if " >> ppCExpr ppc e >> begin >> indent >> newline  >> 
                            ppSPMDCList ppc xs >>  unindent >> end
ppSPMDC ppc (CIf e xs ys) = line "if " >> ppCExpr ppc e >> begin >> indent >> newline >> 
                            ppSPMDCList ppc xs >>  unindent >> end >> 
                            line "else " >> begin >> indent >> newline >> 
                            ppSPMDCList ppc ys >>  unindent >> end 
                            
----------------------------------------------------------------------------
--
ppCExpr :: PPConfig -> CExpr -> PP ()  
ppCExpr ppc (CExpr (CVar nom _)) = line nom
ppCExpr ppc (CExpr (CLiteral v _)) = ppValue v 
ppCExpr ppc (CExpr (CIndex (e,[]) _)) = ppCExpr ppc e 
ppCExpr ppc (CExpr (CIndex (e,xs) _)) = ppCExpr ppc e  >>  
                                        ppCommaSepList (ppCExpr ppc) "[" "]" xs
ppCExpr ppc (CExpr (CCond e1 e2 e3 _))    = ppCExpr ppc e1 >> 
                                            line " ? " >> 
                                            ppCExpr ppc e2 >> 
                                            line " : " >>  
                                            ppCExpr ppc e3
ppCExpr ppc (CExpr (CBinOp bop e1 e2 _)) = line "(" >>  
                                           ppCExpr ppc e1 >> 
                                           ppBinOp bop >> 
                                           ppCExpr ppc e2 >> 
                                           line ")"
ppCExpr ppc (CExpr (CUnOp  uop  e _)) = line "(" >> 
                                        ppUnOp uop >> 
                                        ppCExpr ppc e >> 
                                        line ")" 
ppCExpr ppc (CExpr (CFuncExpr nom args _)) = line nom >> 
                                             ppCommaSepList (ppCExpr ppc) "(" ")" args
ppCExpr ppc (CExpr (CCast e t)) = line "((" >> 
                                  ppCType ppc t >> 
                                  line ")" >> 
                                  ppCExpr ppc e >> 
                                  line ")"


---------------------------------------------------------------------------- 
-- CExpr to Dag and back again. 

{- 
 TODO:  
   + Some things here are clearly faulty. 
     - no regards is taken to scope or code blocks {.. code ... } 
       for example declarations end up within an IF And at the same 
       time the "Computed"-map will say that that variable is computed "globaly"
   + CSE is too brutal. 
      - DONE: I think indexing into a shared memory array should definitely 
              not be stored in a variable. (these two have same access time 
              on the GPU) 

   + Add More detail to the CSEMap. 
      - information about if the declaration of a variable can be moved 
        to toplevel (GLOBAL) or not (LOCAL) 
      - Things are local if they are expressions looking up a value in a shared
        memory array for example or depending on such an expression in any way.   
        Expressions invlving only threadId, BlockId, constants, lengths of global arrays 
        or indexing into global arrays, can be moved to toplevel. (out of all ifs) 
      - Things will be marked as Globally "computed" only if they have been 
        moved out and declared at toplevel.  
        
      


-} 

type CSEMap = Map.Map CExpr (NodeID,CENode)
type Computed = Map.Map NodeID CExpr 

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
cExprToDag cm exp@(CExpr (CVar nom t)) = do
  insertCM cm exp (CENode (CVar nom t))
cExprToDag cm exp@(CExpr (CLiteral l t)) = do 
  insertCM cm exp (CENode (CLiteral l t))
cExprToDag cm exp@(CExpr (CCast e t)) = do 
  (cm1,e') <- cExprToDag cm e
  insertCM cm1 exp (CENode (CCast e' t)) 
  
cExprToDag cm exp@(CExpr (CIndex (e,es) t)) = do 
  (cm1,e') <- cExprToDag cm e
  (cm2,es') <- cExprListToDag cm1 es 
  insertCM cm2 exp (CENode (CIndex (e',es') t))

cExprToDag cm exp@(CExpr (CBinOp op e1 e2 t)) = do   
  (cm1,i1) <- cExprToDag cm e1
  (cm2,i2) <- cExprToDag cm1 e2 
  insertCM cm2 exp (CENode (CBinOp op i1 i2 t))

cExprToDag cm exp@(CExpr (CUnOp op e t)) = do    
  (cm1,i1) <- cExprToDag cm e
  insertCM cm1 exp (CENode (CUnOp op i1 t))

cExprToDag cm exp@(CExpr (CFuncExpr nom es t)) = do    
  (cm1,es1) <- cExprListToDag cm es
  insertCM cm1 exp (CENode (CFuncExpr nom es1 t))

  
cExprListToDag :: CSEMap -> [CExpr]  -> State NodeID (CSEMap,[NodeID])                  
cExprListToDag cm [] = return (cm,[])
cExprListToDag cm (x:xs) = do 
  (cm', xs') <- cExprListToDag cm xs 
  (cmEnd,x') <- cExprToDag cm' x 
  return (cmEnd, x':xs')


type DoneMap = Map.Map CExpr NodeID

performCSE :: [SPMDC] -> [SPMDC] 
performCSE sp = let (_,_,_,r) = performCSEGlobal Map.empty 0 Map.empty sp
                in r
  
performCSEGlobal :: CSEMap -> NodeID -> Computed -> [SPMDC] -> (CSEMap,NodeID,Computed,[SPMDC])
performCSEGlobal cm n cp [] = (cm,n,cp,[]) 
performCSEGlobal cm n cp (p:ps) = (cma,nid,cpn,spcmds ++ prg)
  where 
    (spcmds,(newnid,cm',cp')) = runState (performCSE' p) (n,cm,cp)
    (cma,nid,cpn,prg) = performCSEGlobal cm' newnid cp' ps
                            
performCSE' :: SPMDC -> State (NodeID,CSEMap,Computed) [SPMDC]
performCSE' CSync = return [CSync]
performCSE' c@(CDeclAssign _ _ _) = return [c]
performCSE' (CAssign nom es e) = do 
  (n,cm,cp) <- get
  let ((cm',nid),n') = runState (cExprToDag cm e) n 
      ((cm'',nids),n'') = buildDagList cm' es n'
      elemList = Map.elems cm'' -- pay attention
      (cp',decls,newExp) = dagToSPMDC  elemList cp nid   
      (cp'',moredecls,exps) = dagListToSPMDC elemList cp' nids
  put (nid+1,cm',cp') 
  return (decls ++ moredecls ++ [CAssign nom exps newExp])
performCSE' (CIf b sp1 sp2) = do 
  sp1' <- mapM performCSE' sp1 
  sp2' <- mapM performCSE' sp2 
  return [CIf b (concat sp1') (concat sp2')]
performCSE' a@(CFunc nom es) = return [a]
-- performCSE' apa = error$ show apa 


buildDag cm e n = runState (cExprToDag cm e) n

buildDagList cm [] n = ((cm,[]),n)
buildDagList cm (e:es) n = ((cm'', nid:nids), n'')
  where 
    ((cm',nid),n') = buildDag cm e n
    ((cm'',nids), n'') = buildDagList cm' es n'  

dagToSPMDC idl cp nid =
  case Map.lookup nid cp of 
    (Just expr) -> (cp,[],expr)
    Nothing -> 
      case lookup nid idl of 
        (Just (CENode (CVar nom t))) -> (cp,[], cVar nom t)
        (Just (CENode (CLiteral l t))) -> (cp,[], cLiteral l t) 
        (Just (CENode (CFuncExpr nom args t))) -> 
          (Map.insert nid newExpr cp1,decs++[newDecl],newExpr )
          where 
            newExpr = cVar ("imm" ++show nid ) t
            newDecl = cDeclAssign t ("imm" ++ show nid) (cFuncExpr nom args' t) 
            (cp1,decs,args') = dagListToSPMDC idl cp args
         
        (Just (CENode (CBinOp op e1 e2 t))) -> 
          (Map.insert nid newExpr cp2,decs++[newDecl],newExpr)
          where 
            newExpr = cVar ("imm" ++ show nid) t
            newDecl = cDeclAssign t ("imm" ++ show nid) (cBinOp op e1' e2' t)
            (cp1,d1',e1') = dagToSPMDC idl cp e1 
            (cp2,d2',e2') = dagToSPMDC idl cp1 e2 
            decs = d1' ++ d2'
        (Just (CENode (CUnOp op e t))) -> 
          (Map.insert nid newExpr cp1,decs++[newDecl],newExpr)
          where 
            newExpr = cVar ("imm" ++ show nid) t
            newDecl = cDeclAssign t ("imm" ++ show nid) (cUnOp op e'  t)
            (cp1,d',e') = dagToSPMDC idl cp e
            decs = d'
        {- 
        (Just (CENode (CIndex (e1,es) t))) ->         
          (Map.insert nid newExpr cp2,decs++[newDecl],newExpr)
          where 
            newExpr = cVar ("imm" ++ show nid) t
            newDecl = cDeclAssign t ("imm" ++ show nid) (cIndex (e1',es') t)
            (cp1,d1',e1') = dagToSPMDC idl cp e1
            (cp2,d2',es')  = dagListToSPMDC idl cp1 es -- map (dagToSPMDC idl cp1) es 
         
            decs =     d1' ++ d2' 
        -} 
        -- Do not waste register space for stuff already in shared mem
        (Just (CENode (CIndex (e1,es) t))) ->         
          (cp2,decs,cIndex (e1',es') t)
          where 
            --newExpr = cVar ("imm" ++ show nid) t
            --newDecl = cDeclAssign t ("imm" ++ show nid) (cIndex (e1',es') t)
            (cp1,d1',e1') = dagToSPMDC idl cp e1
            (cp2,d2',es')  = dagListToSPMDC idl cp1 es -- map (dagToSPMDC idl cp1) es 
         
            decs =     d1' ++ d2' 

        (Just (CENode (CCast e t))) -> 
          -- Does this do what I hope ?
          (cp',d',newExpr) 
          where 
            newExpr = cCast e' t
            (cp',d',e') = dagToSPMDC idl cp e
        (Just (CENode (CCond e1 e2 e3 t))) -> 
          (Map.insert nid newExpr cp3,d1' ++ d2' ++ d3' ++ [newDecl],newExpr)
          where 
            newExpr = cVar ("imm" ++ show nid) t 
            newDecl = cDeclAssign t ("imm" ++ show nid) (cCond e1' e2' e3' t)
            (cp1,d1',e1') = dagToSPMDC idl cp e1
            (cp2,d2',e2') = dagToSPMDC idl cp1 e2
            (cp3,d3',e3') = dagToSPMDC idl cp2 e3
        Nothing -> error$ "\n" ++ show nid ++ "\n"  ++ show (map fst idl)
          

dagListToSPMDC idl cp [] = (cp,[],[])
dagListToSPMDC idl cp (x:xs) = (cp'',decs ++ moredecs, exp:exps)
  where 
    (cp',decs,exp) = dagToSPMDC idl cp x 
    (cp'',moredecs, exps) = dagListToSPMDC idl cp' xs 

snd3 (_,y,_) = y
trd3 (_,_,z) = z

----------------------------------------------------------------------------
-- 




-- old printing 
{- 
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
printSPMDC (CDeclAssign t n e) = printCType t ++ " " ++ n ++ " = " ++ printCExpr e ++ ";\n" 
printSPMDC (CFunc nom args) = nom ++ commaSepList printCExpr "(" ")" args ++ ";\n"
printSPMDC CSync  = "__syncthreads();\n"
printSPMDC (CIf e [] [] ) = "" -- 
printSPMDC (CIf e xs [] ) =  "if " ++ printCExpr e ++ "{\n" ++ concatMap printSPMDC xs ++ "}\n"
printSPMDC (CIf e xs ys ) =  "if " ++ printCExpr e ++ "{\n" ++ concatMap printSPMDC xs ++ "}\n" ++ 
                                 "else {\n" ++ concatMap printSPMDC ys ++ "}\n"


printCExpr :: CExpr -> String 
printCExpr (CExpr (CVar nom _)) = nom
printCExpr (CExpr (CLiteral v _)) = printValue v 
printCExpr (CExpr (CIndex (e,[]) _)) = printCExpr e 
printCExpr (CExpr (CIndex (e,xs) _)) = printCExpr e  ++ commaSepList printCExpr "[" "]" xs
printCExpr (CExpr (CCond e1 e2 e3 _))    = printCExpr e1 ++ " ? " ++ printCExpr e2 ++ " : " ++ printCExpr e3
printCExpr (CExpr (CBinOp bop e1 e2 _)) = "(" ++ printCExpr e1 ++ printBinOp bop ++ printCExpr e2 ++ ")"
printCExpr (CExpr (CUnOp  uop  e _)) = "(" ++ printUnOp uop ++ printCExpr e ++ ")" 
printCExpr (CExpr (CFuncExpr nom args _)) = nom ++ commaSepList printCExpr "(" ")" args
printCExpr (CExpr (CCast e t)) = "((" ++ printCType t ++ ")" ++ printCExpr e ++ ")"

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
-} 
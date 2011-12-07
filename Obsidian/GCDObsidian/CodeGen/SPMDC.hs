module Obsidian.GCDObsidian.CodeGen.SPMDC where


import Obsidian.GCDObsidian.Globs
import Obsidian.GCDObsidian.DimSpec

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

               -- Threads, Blocks, Grids (All of type Word32) 
               | CBlockIdx  DimSpec 
               | CThreadIdx DimSpec
               | CBlockDim  DimSpec
               | CGridDim   DimSpec
               
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

cBlockIdx = cexpr1 CBlockIdx
cThreadIdx = cexpr1 CThreadIdx
cBlockDim  = cexpr1 CBlockDim
cGridDim  = cexpr1 CGridDim 
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
ppSPMDC ppc (CAssign e [] expr) = ppCExpr ppc e >> 
                                  line " = " >> 
                                  ppCExpr ppc expr >> 
                                  cTermLn
ppSPMDC ppc (CAssign e exprs expr) = ppCExpr ppc e >> 
                                     ppCommaSepList (ppCExpr ppc) "[" "]" exprs >> 
                                     line " = " >> 
                                     ppCExpr ppc expr >> 
                                     cTermLn 
ppSPMDC ppc (CDeclAssign t n e) = ppCType ppc t >> space >> line n >> line " = " >> ppCExpr ppc e >> cTermLn
ppSPMDC ppc (CFunc nom args) = line nom >> ppCommaSepList (ppCExpr ppc) "(" ")" args >> cTermLn
ppSPMDC ppc  CSync = line (ppSyncLine ppc) >> cTermLn 
ppSPMDC ppc (CIf e [] []) = return ()
ppSPMDC ppc (CIf e xs []) = line "if " >> 
                            wrap "(" ")" (ppCExpr ppc e) >> 
                            begin >> indent >> newline  >> 
                            ppSPMDCList ppc xs >>  unindent >> end
ppSPMDC ppc (CIf e xs ys) = line "if " >> 
                            wrap "(" ")" (ppCExpr ppc e) >> 
                            begin >> indent >> newline >> 
                            ppSPMDCList ppc xs >>  unindent >> end >> 
                            line "else " >> begin >> indent >> newline >> 
                            ppSPMDCList ppc ys >>  unindent >> end 
                            
----------------------------------------------------------------------------
--
ppCExpr :: PPConfig -> CExpr -> PP ()  
-- Cheat and do CUDA print for now!
  -- should do lookup in PPConfig and figure out how to 
  -- print these for CUDA/OpenCL
ppCExpr ppc (CExpr (CBlockIdx X)) = line "blockIdx.x" 
ppCExpr ppc (CExpr (CBlockIdx Y)) = line "blockIdx.y" 
ppCExpr ppc (CExpr (CBlockIdx Z)) = line "blockIdx.z" 
ppCExpr ppc (CExpr (CThreadIdx X)) = line "threadIdx.x" 
ppCExpr ppc (CExpr (CThreadIdx Y)) = line "threadIdx.y" 
ppCExpr ppc (CExpr (CThreadIdx Z)) = line "threadIdx.z" 
ppCExpr ppc (CExpr (CBlockDim X)) = line "blockDim.x" 
ppCExpr ppc (CExpr (CBlockDim Y)) = line "blockDim.y" 
ppCExpr ppc (CExpr (CBlockDim Z)) = line "blockDim.z" 
ppCExpr ppc (CExpr (CGridDim X)) = line "gridDim.x" 
ppCExpr ppc (CExpr (CGridDim Y)) = line "gridDim.y" 
ppCExpr ppc (CExpr (CGridDim Z)) = line "gridDim.z" 

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
   + IN PROGRESS: Some things here are clearly faulty. 
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
      - What about conditional blocks. 
        Atleast Array indexing inside such a block should not be moved. 
        
      


-} 

type CSEMap = Map.Map CExpr (NodeID,CENode,Integer)
-- type CSEMap = Map.Map CExpr (NodeID,CENode)
type Computed = Map.Map NodeID CExpr 


----------------------------------------------------------------------------
newNodeID = do 
  i <- get 
  put (i+1)
  return i

----------------------------------------------------------------------------
insertCM :: CSEMap -> CExpr -> CENode -> State NodeID (CSEMap,NodeID) 
insertCM cm expr node = 
  case Map.lookup expr cm of 
    (Just (i,n,m)) -> 
      let cm' = Map.insert expr (i,n,m+1) cm
      in return (cm', i)
    Nothing  -> do
      i <- newNodeID 
      let cm' = Map.insert expr (i,node,1) cm 
      return (cm',i)
  
----------------------------------------------------------------------------
-- Find expressions that can be computed once globally 

globalName nom = not (List.isPrefixOf "arr" nom)

isGlobal (CExpr (CBlockIdx a)) = True
isGlobal (CExpr (CThreadIdx a)) = True
isGlobal (CExpr (CBlockDim a)) = True
isGlobal (CExpr (CGridDim a)) = True 
isGlobal (CExpr (CVar nom _)) = globalName nom
isGlobal (CExpr (CLiteral l _)) = True 
isGlobal (CExpr (CCast e _)) = isGlobal e
isGlobal (CExpr (CIndex (e,es) _)) = False -- isGlobal e && (all isGlobal es) 
isGlobal (CExpr (CBinOp _ e1 e2 _)) = isGlobal e1 && isGlobal e2
isGlobal (CExpr (CUnOp _ e _)) = isGlobal e
isGlobal (CExpr (CFuncExpr nom es _)) = all isGlobal es
  
----------------------------------------------------------------------------
cExprToDag :: CSEMap -> CExpr -> State NodeID (CSEMap,NodeID) 
cExprToDag cm exp@(CExpr (CBlockIdx a)) = 
  insertCM cm exp (CENode (CBlockIdx a)) 
cExprToDag cm exp@(CExpr (CThreadIdx a)) = 
  insertCM cm exp (CENode (CThreadIdx a)) 
cExprToDag cm exp@(CExpr (CBlockDim a)) = 
  insertCM cm exp (CENode (CBlockDim a)) 
cExprToDag cm exp@(CExpr (CGridDim a)) = 
  insertCM cm exp (CENode (CGridDim a)) 
  
cExprToDag cm exp@(CExpr (CVar nom t)) = 
  insertCM cm exp (CENode (CVar nom t)) 
cExprToDag cm exp@(CExpr (CLiteral l t)) =  
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

----------------------------------------------------------------------------
cExprListToDag :: CSEMap -> [CExpr]  -> State NodeID (CSEMap,[NodeID])                  
cExprListToDag cm [] = return (cm,[])
cExprListToDag cm (x:xs) = do 
  (cm', xs') <- cExprListToDag cm xs 
  (cmEnd,x') <- cExprToDag cm' x 
  return (cmEnd, x':xs')

----------------------------------------------------------------------------
type DoneMap = Map.Map CExpr NodeID

----------------------------------------------------------------------------
{-
performCSE :: [SPMDC] -> [SPMDC] 
performCSE sp = let (_,_,_,r) = performCSEGlobal Map.empty 0 Map.empty sp
                in r
  -}
----------------------------------------------------------------------------
{- 
performCSEGlobal :: CSEMap 
                    -> NodeID 
                    -> Computed 
                    -> [SPMDC] 
                    -> (CSEMap,NodeID,Computed,[SPMDC])
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
  (n,cm,cp) <- get
  let ((cm',nid),n') = runState (cExprToDag cm b) n 
      elemList = Map.elems cm'
      (cp',decls,newExp) = dagToSPMDC elemList cp nid
  put (nid+1,cm',cp') 
  sp1' <- mapM performCSE' sp1 
  sp2' <- mapM performCSE' sp2 
  return$ decls ++ [CIf newExp (concat sp1') (concat sp2')]
performCSE' a@(CFunc nom es) = return [a]
-} 

buildDag cm e n = runState (cExprToDag cm e) n

buildDagList cm [] n = ((cm,[]),n)
buildDagList cm (e:es) n = ((cm'', nid:nids), n'')
  where 
    ((cm',nid),n') = buildDag cm e n
    ((cm'',nids), n'') = buildDagList cm' es n'  

dagToSPMDC :: [(NodeID,CENode)] -> Computed -> NodeID -> (Computed,[SPMDC],CExpr)
dagToSPMDC idl cp nid =
  case Map.lookup nid cp of 
    (Just expr) -> (cp,[],expr)
    Nothing -> 
      case lookup nid idl of 
        (Just (CENode (CBlockIdx a))) -> (cp, [], cBlockIdx a)
        (Just (CENode (CThreadIdx a))) -> (cp, [], cThreadIdx a)
        (Just (CENode (CBlockDim a))) -> (cp, [], cBlockDim a)
        (Just (CENode (CGridDim a))) -> (cp, [], cGridDim a)
        
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
----------------------------------------------------------------------------
-- 


buildCSEMap :: [SPMDC] -> CSEMap 
buildCSEMap sps = snd$  buildCSEMap' (Map.empty) 0 sps 
  where 
    
buildCSEMap' cm n [] = (n,cm) 
buildCSEMap' cm n (sp:sps) =  buildCSEMap' cm' n' sps
  where 
    (n',cm') = (collectCSE cm n sp)

    
collectCSE cm n CSync = (n,cm)
collectCSE cm n (CDeclAssign _ _ _) = error "CDeclAssign found during collectCSE"
collectCSE cm n (CAssign nom es e) = 
  let ((cm',nid),n') = runState (cExprToDag cm e) n 
      ((cm'',nids),n'') = buildDagList cm' es n'
  in (n'',cm'')
     

collectCSE cm n (CIf b sp1 sp2) = (n3,cm3)
  where 
    ((cm1,nid),n1) = runState (cExprToDag cm b) n 
    (n2,cm2) = buildCSEMap' cm1 n1 sp1
    (n3,cm3) = buildCSEMap' cm2 n2 sp2 
collectCSE cm n (CFunc nom es) = (n1,cm1)
  where 
    ((cm1,nids),n1) = buildDagList cm es n


performCSE2 :: [SPMDC] -> [SPMDC] 
performCSE2 sps = test ++ r
  where 
    cseMap = buildCSEMap sps -- map containing all expressions 
    -- globs  = getGlobals cseMap
    (cp,test)   = declareGlobals cseMap
    
    r = performCSEPass cseMap cp sps 
    --(_,_,_,r) = performCSEGlobal cseMap 0 cp sps
                
-- Extract things that can be computed early
    
declareGlobals :: CSEMap -> (Computed, [SPMDC]) 
declareGlobals cm = declareGlobals' (Map.empty) globs 
  where 
    getGlobals :: CSEMap -> [(NodeID,CENode,Integer)]
    getGlobals cm = globs
      where 
        globs = Map.elems cm' 
        cm'   = Map.filterWithKey (\k e -> isGlobal k) cm

    globs = getGlobals cm 
    strip = map (\(x,y,z) -> (x,y))
    declareGlobals' cp []  = (cp,[]) 
    declareGlobals' cp ((nid,cenode,i):xs) = 
      if (i >= 2) 
      then
        case Map.lookup nid cp of
          (Just e) -> declareGlobals' cp xs          
          Nothing -> 
            let (cp',sps,e) = dagToSPMDC (strip globs) cp nid 
                (cp'',sps2) = declareGlobals' cp' xs
            in (cp'',sps ++ sps2)
      else declareGlobals' cp xs          
           
           
      
--dagToSPMDC :: [(NodeID,CENode)] -> Computed -> NodeID -> (Computed,[SPMDC],CExpr)

performCSEPass :: CSEMap -> Computed -> [SPMDC] -> [SPMDC]                             
performCSEPass cm cp [] = []
performCSEPass cm cp (x:xs) = performCSEPass' cm cp x : performCSEPass cm cp xs 

-- Does not add any new declarations (Maybe will later)              
performCSEPass' :: CSEMap -> Computed -> SPMDC -> SPMDC
performCSEPass' cm cp CSync = CSync
performCSEPass' cm cp (CDeclAssign _ _ _) = error "CDeclAssign found during CSEPass" 
performCSEPass' cm cp (CAssign nom es e)  = CAssign nom xs x 
  where
    (x:xs) = cseReplaceL cm cp (e:es) 
performCSEPass' cm cp (CIf b sp1 sp2) = CIf b' (performCSEPass cm cp sp1) 
                                               (performCSEPass cm cp sp2)
  where 
    b' = cseReplace cm cp b 
performCSEPass' cm cp a@(CFunc nom es) = a -- look


----------------------------------------------------------------------------
cseReplaceL cm cp [] = []
cseReplaceL cm cp (x:xs) = cseReplace cm cp x: cseReplaceL cm cp xs


cseReplace cm cp exp@(CExpr (CIndex (e,es) t))  = 
  case Map.lookup exp cm of 
    (Just (nid,node,_)) ->
      case Map.lookup nid cp of 
        (Just exp') -> exp' 
        Nothing -> CExpr (CIndex (cseReplace cm cp e,
                                  cseReplaceL cm cp es) t)
    Nothing -> error "cseReplace: expression missing from CSEMap"
cseReplace cm cp exp@(CExpr (CCast e t)) = 
  case Map.lookup exp cm of 
    (Just (nid,node,_)) ->
      case Map.lookup nid cp of 
        (Just exp') -> exp' 
        Nothing -> CExpr (CCast (cseReplace cm cp e) t)                                 
    Nothing -> error "cseReplace: expression missing from CSEMap"
cseReplace cm cp exp@(CExpr (CBinOp op e1 e2 t)) = 
  case Map.lookup exp cm of 
    (Just (nid,node,_)) ->
      case Map.lookup nid cp of 
        (Just exp') -> exp' 
        Nothing -> CExpr (CBinOp op (cseReplace cm cp e1) 
                                    (cseReplace cm cp e2) t)                                 
    Nothing -> error "cseReplace: expression missing from CSEMap"                                        
cseReplace cm cp exp@(CExpr (CUnOp op e t)) = 
  case Map.lookup exp cm of 
    (Just (nid,node,_)) ->
      case Map.lookup nid cp of 
        (Just exp') -> exp' 
        Nothing -> CExpr (CUnOp op (cseReplace cm cp e) t)                                 
    Nothing -> error "cseReplace: expression missing from CSEMap"                                        
cseReplace cm cp exp@(CExpr (CFuncExpr nom es t)) = 
  case Map.lookup exp cm of 
    (Just (nid,node,_)) ->
      case Map.lookup nid cp of 
        (Just exp') -> exp' 
        Nothing -> CExpr (CFuncExpr nom (cseReplaceL cm cp es) t)                                 
    Nothing -> error "cseReplace: expression missing from CSEMap"                                        
cseReplace cm cp exp = 
  case Map.lookup exp cm of 
    (Just (nid,node,_)) ->  
      case Map.lookup nid cp of 
        (Just exp') -> exp'
        Nothing     -> exp 
    Nothing -> error "cseReplace: expression missing from CSEMap"                                        
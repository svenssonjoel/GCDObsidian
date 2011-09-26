{-# LANGUAGE RankNTypes, GADTs #-}

module Obsidian.GCDObsidian.CodeGen.Common where 

import Data.List
import Data.Word
import qualified Data.Map as Map 

import Obsidian.GCDObsidian.Kernel 
import Obsidian.GCDObsidian.Exp 
import qualified Obsidian.GCDObsidian.Tuple as Tuple 
import Obsidian.GCDObsidian.Tuple (Tuple ((:.),Nil) ) 
import Obsidian.GCDObsidian.Elem

import Control.Monad.State


------------------------------------------------------------------------------
-- TINY TOOLS 
fst2 (x,y,z) = (x,y) 

------------------------------------------------------------------------------ 
data GenConfig = GenConfig { global :: String,
                             local  :: String };
  
genConfig = GenConfig


------------------------------------------------------------------------------
-- Helpers 
mappedName :: Name -> Bool 
mappedName name = isPrefixOf "arr" name


genType _ Int = "int "
genType _ Float = "float "
genType _ Double = "double "
genType _ Bool = "int " 
genType _ Word8 = "uint8_t "
genType _ Word16 = "uint16_t "
genType _ Word32 = "uint32_t "
genType _ Word64 = "uint64_t " 
genType gc (Pointer t) = genType gc t ++ "*"
genType gc (Global t) = global gc ++" "++ genType gc t  -- "__global " ++ genType t
genType gc (Local t)  = local gc  ++" "++ genType gc t 

genCast gc t = "(" ++ genType gc t ++ ")"

parens s = "(" ++ s ++ ")"

------------------------------------------------------------------------------
-- genExp C-style 
genExp :: Elem a => GenConfig -> MemMap -> Exp a -> [String]
genExp gc _ (Literal a) = [show a] 
genExp gc _ (Index (name,[])) = [name]
genExp gc mm exp@(Index (name,es)) = 
  [name' ++ genIndices gc mm es]
  where 
    (offs,t)  = 
      case Map.lookup name mm of  
        Nothing -> error "array does not excist in map" 
        (Just x) -> x
    name' = if mappedName name 
            then parens$ genCast gc t ++ 
                 if offs > 0 
                 then "(sbase+" ++ show offs ++ ")"             
                 else "sbase"
            else name

   
                 
genExp gc mm (Op op e) = [genOp op (genExp gc mm e)]
genExp gc mm (Tuple t) = genTup gc mm t

genIndices gc mm es = concatMap (pIndex mm) es  
  where 
    pIndex mm e = "[" ++ concat (genExp gc mm e) ++ "]"


------------------------------------------------------------------------------
-- genOp
genOp :: Op a -> [String] -> String
genOp Add     [a,b] = oper "+" a b 
genOp Sub     [a,b] = oper "-" a b 
genOp Mul     [a,b] = oper "*" a b 
genOp Div     [a,b] = oper "/" a b 

genOp If      [b,e1,e2] = b ++ " ? " ++ e1 ++ " : " ++ e2

genOp Sin     [a]   = func "sin" a 

-- Bool ops
genOp Eq      [a,b] = oper "==" a b 
genOp Lt      [a,b] = oper "<" a b 
genOp LEq     [a,b] = oper "<=" a b 
genOp Gt      [a,b] = oper ">" a b
genOp GEq     [a,b] = oper ">=" a b

-- Bitwise ops
genOp BitwiseAnd [a,b] = oper "&" a b 
genOp BitwiseOr  [a,b] = oper "|" a b 
genOp BitwiseXor [a,b] = oper "^" a b 
genOp BitwiseNeg [a]   = unOp "~" a 
genOp ShiftL     [a,b] = oper "<<" a b 
genOp ShiftR     [a,b] = oper ">>" a b 


-- built-ins 
genOp Min      [a,b] = func "min" (a ++ "," ++ b) 
genOp Max      [a,b] = func "max" (a ++ "," ++ b) 

func  f a = f ++ "(" ++ a ++ ")" 
oper  f a b = "(" ++ a ++ f ++ b ++ ")" 
unOp  f a   = "(" ++ f ++ a ++ ")"


genTup :: forall t. GenConfig -> MemMap -> Tuple.Tuple Exp t -> [String]
genTup _  _ Nil = []
genTup gc mm (a :. t) = genExp gc mm a ++ (genTup gc mm t) 
  
genPrj = undefined 


------------------------------------------------------------------------------
-- print and indent and stuff... 
--  This is probably very ugly 

-- TODO: There is a chapter about this (PP) in "implementing functional lang..." 
--       Look at that and learn 


type PP a = State (Int,String) a  

indent :: PP ()
indent = 
  do 
    (i,s) <- get 
    put (i+1,s) 
    
unindent :: PP () 
unindent = 
  do 
    (i,s) <- get 
    if i <= 0 then error "Whats going on" else put (i-1,s) 

line :: String -> PP () 
line str = 
  do 
    (i,s) <- get 
    put (i,s ++ str) 

  

newline :: PP () 
newline = 
  do 
    (i,s) <- get 
    let ind = replicate (i*2) ' '
    put (i,s ++ "\n" ++ ind)
    
runPP :: PP a -> Int -> String
runPP pp i = snd$ execState pp (i,"")



------------------------------------------------------------------------------
-- Configurations, threads,memorymap 

data Config = Config {configThreads  :: NumThreads, 
                      configMM       :: MemMap,
                      configLocalMem :: Word32} 
config = Config


assign :: Elem a => GenConfig -> MemMap -> Exp a -> Exp a -> PP () 
assign gc mm name val = line ((concat (genExp gc mm name)) ++ 
                           " = " ++  concat (genExp gc mm val) ++ 
                           ";") 
                                                    
cond :: GenConfig -> MemMap -> Exp Bool -> PP ()  
cond gc mm e = line ("if " ++ concat (genExp gc mm e))  

begin :: PP () 
begin = line "{" >> indent >> newline

end :: PP () 
end =  unindent >> newline >> line "}" >> newline



------------------------------------------------------------------------------
-- First Synchtreads analysis:
-- This code is needed but both OpenCL and CUDA so 
-- therefore in Common.hs
    
data Syncthreads = Syncthreads {needsSync :: Bool}
                 deriving Show
                          
syncthreads = Syncthreads True    
nosync      = Syncthreads False
                     
-- Performs no analysis, just says "yes, we need a sync" everywhere. 
syncPoints :: Code a -> Code Syncthreads
syncPoints Skip = Skip
syncPoints ((SyncUnit nt ps e) `Seq` code) = 
   SyncUnit nt ps syncthreads  `Seq` (syncPoints code)
  
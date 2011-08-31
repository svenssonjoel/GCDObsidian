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
{- 
  This module contains some function to print an Exp as a 
  C-Style code. Used by CUDA and OpenCL code generation 

-} 


------------------------------------------------------------------------------
-- Helpers 
mappedName :: Name -> Bool 
mappedName name = isPrefixOf "arr" name


genType Int = "int"
genType Float = "float"
genType Double = "double"
genType Bool = "int" 
genType Word8 = "uint8_t"
genType Word16 = "uint16_t"
genType Word32 = "uint32_t"
genType Word64 = "uint64_t" 
genType (Pointer t) = genType t ++ " *"
genType (Global t) = "__global " ++ genType t

genCast  t = "(" ++ genType t ++ ")"

parens s = "(" ++ s ++ ")"

------------------------------------------------------------------------------
-- genExp C-style 
genExp :: Elem a => MemMap -> Exp a -> [String]
genExp _ (Literal a) = [show a] 
genExp _ (Index (name,[])) = [name]
genExp mm exp@(Index (name,es)) = 
  [name' ++ genIndices mm es]
  where 
    (offs,t)  = 
      case Map.lookup name mm of  
        Nothing -> error "array does not excist in map" 
        (Just x) -> x
    name' = if mappedName name 
            then parens$ genCast t ++ 
                 if offs > 0 
                 then "(sbase+" ++ show offs ++ ")"             
                 else "sbase"
            else name

   
                 
genExp mm (Op op e) = [genOp op (genExp mm e)]
genExp mm (Tuple t) = genTup mm t

genIndices mm es = concatMap (pIndex mm) es  
  where 
    pIndex mm e = "[" ++ concat (genExp mm e) ++ "]"


------------------------------------------------------------------------------
-- genOp
genOp :: Op a -> [String] -> String
genOp Add     [a,b] = oper "+" a b 
genOp Sub     [a,b] = oper "-" a b 
genOp Mul     [a,b] = oper "*" a b 
genOp Sin     [a]   = func "sin" a 

-- Bool ops
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

func  f a = f ++ "(" ++ a ++ ")" 
oper  f a b = "(" ++ a ++ f ++ b ++ ")" 
unOp  f a   = "(" ++ f ++ a ++ ")"

 


genTup :: forall t. MemMap -> Tuple.Tuple Exp t -> [String]
genTup _ Nil = []
genTup mm (a :. t) = genExp mm a ++ (genTup mm t) 
  
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
-- First Synchtreads analysis 
    
data Syncthreads = Syncthreads {needsSync :: Bool}
                 deriving Show
syncthreads = Syncthreads True    
nosync      = Syncthreads False
                   
syncPoints :: Code a -> Code Syncthreads
syncPoints Skip = Skip
syncPoints (store `Seq` code) = syncPointsStore store `Seq` 
                                syncPoints code
                                
                                
storeNeedsSync :: Store Syncthreads -> Bool                                 
storeNeedsSync (Store _ ws) = any writeNeedsSync ws


syncPointsStore :: Store a -> Store Syncthreads
syncPointsStore (Store nt ws) = Store nt (map syncPointsWrite ws)

writeNeedsSync :: Write Syncthreads -> Bool 
writeNeedsSync (Write _ _ s) = needsSync s

syncPointsWrite :: Write a -> Write Syncthreads
syncPointsWrite (Write targ arr _) = 
  Write targ arr syncthreads
  
{-# LANGUAGE     RankNTypes, 
                 GADTs #-} 
module Obsidian.GCDObsidian.Program 
       (Program(..)
       ,printProgram
       ,programThreads
       ,(*>*)
       ,targetArray
       ,targetPair
       )where 

import Data.Word

import Obsidian.GCDObsidian.Exp
import Obsidian.GCDObsidian.Types
import Obsidian.GCDObsidian.Globs

----------------------------------------------------------------------------
-- Scalars for now. Learn how to loosen that demand
-- TODO: really need to loosen it ??? 
-- TODO: This might be related to the issue of having Tuples 
--       in the Exp type or not... Look into this! 
-- TODO: Program type can represent a whole lot more kinds of programs 
--       than those that we actually generate.
data Program 
  = forall a. Scalar a => Assign Name (Data Word32) (Data a)
  | ForAll (Data Word32 -> Program) Word32
  | Allocate Name Word32 Type Program 
  | Cond (Exp Bool) Program                     -- Conditional such as if (tid < x) (assign bla bla)  
  | ProgramSeq Program Program  
  
(*>*) :: Program -> Program -> Program     
(*>*) = ProgramSeq 
    
programThreads :: Program -> Word32
programThreads (Assign _ _ _) = 1
programThreads (ForAll f n) = n -- inner ForAlls are sequential
programThreads (Allocate _ _ _ p) = programThreads p 
programThreads (p1 `ProgramSeq` p2) = max (programThreads p1) (programThreads p2)
    
printProgram :: Program -> String 
printProgram (Assign n t e) = n ++ "[" ++ show t ++ "]" ++ " = " ++ show e ++ ";\n"  
printProgram (ForAll f n)   = "forall i 0.." ++ show n ++ " {\n" ++ printProgram (f (variable "tid")) ++ "\n}" 
printProgram (Allocate name n t p) = name ++ " = malloc(" ++ show n ++ ")\n" ++ printProgram p
printProgram (ProgramSeq p1 p2) = printProgram p1 ++ printProgram p2
    
instance Show Program where 
  show = printProgram 




targetArray :: Scalar a => Name -> Exp Word32 -> (Exp a -> Program)
targetArray n i = \a -> Assign n i a 


targetPair :: (Scalar a, Scalar b) => Name -> Name -> Exp Word32 -> ((Exp a,Exp b) -> Program)
targetPair n1 n2  i = \(a,b) -> Assign n1 i a *>*
                                  Assign n2 i b

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
-- 

data Program extra 
  = forall a. Scalar a => Assign Name (Data Word32) (Data a)
  | ForAll (Data Word32 -> (Program extra)) Word32
  | Allocate Name Word32 Type (Program extra) 
  | Cond (Exp Bool) (Program extra)       -- Conditional such as if (tid < x) (assign bla bla)  
  | ProgramSeq (Program extra) 
               (Program extra) 
  
(*>*) :: Program extra 
         -> Program extra 
         -> Program extra    
(*>*) = ProgramSeq 
    
programThreads :: Program extra -> Word32
programThreads (Assign _ _ _) = 1
programThreads (ForAll f n) = n -- inner ForAlls are sequential
programThreads (Allocate _ _ _ p) = programThreads p 
programThreads (p1 `ProgramSeq` p2) = max (programThreads p1) (programThreads p2)
    
                                      
printProgram :: Show extra => Program extra -> String 
printProgram (Assign n t e) = n ++ "[" ++ show t ++ "]" ++ " = " ++ show e ++ ";\n"  
printProgram (ForAll f n)   = "forall i 0.." ++ show n ++ " {\n" ++ printProgram (f (variable "tid")) ++ "\n}" 
printProgram (Allocate name n t p) = name ++ " = malloc(" ++ show n ++ ")\n" ++ printProgram p
printProgram (ProgramSeq p1 p2) = printProgram p1 ++ printProgram p2
    
instance Show extra => Show (Program extra) where 
  show = printProgram 


targetArray :: Scalar a => Name -> Exp Word32 -> (Exp a -> Program ())
targetArray n i = \a -> Assign n i a 


targetPair :: (Scalar a, Scalar b) => Name -> Name -> Exp Word32 -> ((Exp a,Exp b) -> Program ())
targetPair n1 n2  i = \(a,b) -> Assign n1 i a *>*
                                  Assign n2 i b

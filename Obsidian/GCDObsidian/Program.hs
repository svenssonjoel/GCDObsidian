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
  = Skip
    
  | forall a. Scalar a => Assign Name (Data Word32) (Data a) 
-- Note: Writing of a scalar value into an array location.     
  | ForAll (Data Word32 -> (Program extra)) Word32   
-- DONE: I Think Allocate should not introduce nesting
  | Allocate Name Word32 Type extra
  | Synchronize 
-- NOTE: Adding a synchronize statement here 
--       the sync operation can now insert a Syncronizze as a guide 
--       to the code generation 
-- TODO: What will this mean when nested inside something ? 
--       Again, to start with, I will ensure that none of my library function 
--       introduces a Synchronize nested in anything. 
--  | Cond (Exp Bool) (Program extra)           
-- NOTE: Conditional such as if (tid < x) (assign ...) 
-- NOTE: The need for a Cond here is not obvious right now. Remove for now. 
    
  | ProgramSeq (Program extra) 
               (Program extra) 
  
(*>*) :: Program extra 
         -> Program extra 
         -> Program extra    
(*>*) = ProgramSeq 
    
programThreads :: Program extra -> Word32
programThreads Skip = 0
programThreads Synchronize = 0 
programThreads (Assign _ _ _) = 1
programThreads (ForAll f n) = n -- inner ForAlls are sequential
programThreads (Allocate _ _ _ _) = 0 -- programThreads p 
-- programThreads (Cond b p ) = programThreads p
programThreads (p1 `ProgramSeq` p2) = max (programThreads p1) (programThreads p2)
    
                                      
printProgram :: Show extra => Program extra -> String 
printProgram Skip = ";" 
printProgram Synchronize = "Sync\n" 
printProgram (Assign n t e) = n ++ "[" ++ show t ++ "]" ++ " = " ++ show e ++ ";\n"  
printProgram (ForAll f n)   = "forall i 0.." ++ show n ++ " {\n" ++ printProgram (f (variable "tid")) ++ "\n}" 
-- printProgram (Cond b p)     = "cond {\n" ++ printProgram p ++ "\n}"
printProgram (Allocate name n t e) = name ++ " = malloc(" ++ show n ++ ")\n" ++ 
                                     "[*** " ++ show e ++ " ***]\n" 
printProgram (ProgramSeq p1 p2) = printProgram p1 ++ printProgram p2
    
instance Show extra => Show (Program extra) where 
  show = printProgram 


targetArray :: Scalar a => Name -> Exp Word32 -> (Exp a -> Program ())
targetArray n i = \a -> Assign n i a 


targetPair :: (Scalar a, Scalar b) => Name -> Name -> Exp Word32 -> ((Exp a,Exp b) -> Program ())
targetPair n1 n2  i = \(a,b) -> Assign n1 i a *>*
                                  Assign n2 i b

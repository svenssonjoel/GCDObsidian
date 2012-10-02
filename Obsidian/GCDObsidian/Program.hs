{-# LANGUAGE     RankNTypes, 
                 GADTs #-} 
module Obsidian.GCDObsidian.Program 
       (Program(..)
       ,printProgram
       ,programThreads
       ,(*>*)
       ,targetArray
       ,targetPair
       ,Atomic(..)
       ,printAtomic  
       )where 

import Data.Word

import Obsidian.GCDObsidian.Exp
import Obsidian.GCDObsidian.Types
import Obsidian.GCDObsidian.Globs

----------------------------------------------------------------------------
-- 
-- TODO: Data a and Exp a is the same. (look at this!) 
data Program extra 
  = Skip
    
  | forall a. Scalar a => Assign Name (Data Word32) (Data a) 
-- Note: Writing of a scalar value into an array location.     
  | ForAll (Data Word32 -> (Program extra)) Word32   

-- Could this be improved ? 
--  | ForAllGlobal (Data Word32 -> (Program extra)) (Exp Word32)
-- TODO: Maybe the (Exp Word32, Exp Word32) should be (Word32,Word32)
--                   Blocks      blocksize 
  | ForAllGlobal (Exp Word32, Exp Word32)
                 (Exp Word32 -> Exp Word32 -> Program extra)

-- Alternative (but I dont know... this implies there
--              can be things such as Allocate inside a ForAllBlocks..
--              Maybe that is ok, just need to think a bit more about it) 
  | ForAllBlocks (Exp Word32) (Exp Word32 -> Program extra)    
    
-- DONE: I Think Allocate should not introduce nesting
  | Allocate Name Word32 Type extra
-- potentially a sync is needed. 
-- DONE: Analysis will check if it is REALLY needed
  | Synchronize Bool 
-- NOTE: Adding a synchronize statement here 
--       the sync operation can now insert a Syncronizze as a guide 
--       to the code generation 
-- TODO: What will this mean when nested inside something ? 
--       Again, to start with, I will ensure that none of my library function 
--       introduces a Synchronize nested in anything.     
  | ProgramSeq (Program extra) 
               (Program extra) 

  | forall a. Scalar a => AtomicOp Name Name (Exp Word32) (Atomic (Data a))

-- took same as precedence as for ++ for *>*
infixr 5 *>* 

(*>*) :: Program extra 
         -> Program extra 
         -> Program extra    
(*>*) = ProgramSeq 
    
programThreads :: Program extra -> Word32
programThreads Skip = 0
programThreads (Synchronize _) = 0 
programThreads (Assign _ _ _) = 1
programThreads (ForAll f n) = n -- inner ForAlls are sequential
programThreads (Allocate _ _ _ _) = 0 -- programThreads p 
-- programThreads (Cond b p ) = programThreads p
programThreads (p1 `ProgramSeq` p2) = max (programThreads p1) (programThreads p2)
programThreads (AtomicOp _ _ _ _) = 1

                                      
printProgram :: Show extra => Program extra -> String 
printProgram Skip = ";" 
printProgram (Synchronize b) = "Sync " ++ show b ++ "\n" 
printProgram (Assign n t e) = n ++ "[" ++ show t ++ "]" ++ " = " ++ show e ++ ";\n"  
printProgram (ForAll f n)   = "par i " ++ show n ++ " {\n" ++ printProgram (f (variable "i")) ++ "\n}" 
-- printProgram (Cond b p)     = "cond {\n" ++ printProgram p ++ "\n}"
printProgram (Allocate name n t e) = name ++ " = malloc(" ++ show n ++ ")\n" ++ 
                                     "[*** " ++ show e ++ " ***]\n" 
printProgram (ProgramSeq p1 p2) = printProgram p1 ++ printProgram p2
-- Needs fresh name generations to be correct
printProgram (AtomicOp name n i e) = name ++ " = " ++ printAtomic e ++
                                     "(" ++ n ++ "[" ++ show i ++ "])"

instance Show extra => Show (Program extra) where 
  show = printProgram 


targetArray :: Scalar a => Name -> (Exp Word32,Exp a) -> Program ()
targetArray n (i,a) = Assign n i a 


targetPair :: (Scalar a, Scalar b) => Name -> Name -> (Exp Word32,(Exp a,Exp b)) -> Program ()
targetPair n1 n2  (i,(a,b)) = Assign n1 i a *>*
                              Assign n2 i b

-- Atomic operations

data Atomic a where
  AtomicInc :: Atomic (Data Int)

printAtomic AtomicInc = "atomicInc"

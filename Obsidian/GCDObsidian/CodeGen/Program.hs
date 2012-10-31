

{- CodeGen.Program.

   To get somewhere at pace I will transform the new Program monad
   into a backend-program type that is more or less identical to the
   old one (the one that all the code generation expects)

-} 


module Obsidian.GCDObsidian.CodeGen.Program where

import Obsidian.GCDObsidian.Exp
import Obsidian.GCDObsidian.Globs



---------------------------------------------------------------------------
-- Program
--------------------------------------------------------------------------- 
data Program extra
     = Skip
     | forall a. Scalar a =>
       Assign Name (Exp Word32) (Exp a) 
     | forall a. Scalar a =>
       AtomicOp Name Name (Exp Word32) (Atomic (Exp a))
     | ForAll Word32 (Exp Word32 -> Program extra)
       --  | ForAllBlocks
     | Allocate Name Word32 Type extra
     | Synchronize Bool
     | ProgramSeq (Program extra)
                  (Program extra)

---------------------------------------------------------------------------
-- 
---------------------------------------------------------------------------
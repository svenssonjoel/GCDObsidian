module Obsidian.GCDObsidian.ModifyArray where

import Obsidian.GCDObsidian.Exp 
import Obsidian.GCDObsidian.Types
import Obsidian.GCDObsidian.Globs
import Obsidian.GCDObsidian.Program
import Obsidian.GCDObsidian.Array

import Data.Word

{- 
data Modify a = Modify { modFun :: P (Exp Word32), atomicOp :: Atomic a }

type ModArray a = Array Modify a

mkModifyArray n p op = Array n (Modify p op)
-} 
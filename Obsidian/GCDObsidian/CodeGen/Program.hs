{-# LANGUAGE GADTs,
             ExistentialQuantification #-}

{- CodeGen.Program.

   To get somewhere at pace I will transform the new Program datatype
   (monad) into a backend-program type that is more or less identical
   to the old one (the one that all the code generation expects)

-} 


module Obsidian.GCDObsidian.CodeGen.Program where

import Obsidian.GCDObsidian.Exp
import Obsidian.GCDObsidian.Globs
import Obsidian.GCDObsidian.Types
import Obsidian.GCDObsidian.Atomic

import qualified Obsidian.GCDObsidian.Program as P 

import Data.Word
import Data.Supply

import System.IO.Unsafe
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
       -- Not present in old Program datatype
     | ForAllBlocks (Exp Word32) (Exp Word32 -> Program extra)
     | Allocate Name Word32 Type extra
       -- Not Present in old Program datatype 
     | Output Name Type 
     | Synchronize Bool
     | ProgramSeq (Program extra)
                  (Program extra)

---------------------------------------------------------------------------
-- Program translation from (P.Program a) to (Program ()) 
---------------------------------------------------------------------------

runPrg p = snd$ runPrg' ns p 
  where ns = unsafePerformIO$ newEnumSupply 
      
runPrg' :: Supply Int -> P.Program a -> (a,Program ())
runPrg' i (P.Assign name ix e) = ((),Assign name ix e)
runPrg' i (P.AtomicOp name ix at) =
  let nom = "a" ++ show (supplyValue i)
  in  (variable nom,AtomicOp  nom name ix at) 
runPrg' i (P.ForAll n f) =
  let newf = (\x -> snd (runPrg' i (f x)))
  in  ((),ForAll n newf)
runPrg' i (P.ForAllBlocks n f) =
  let newf = (\x -> snd (runPrg' i (f x)))
  in ((),ForAllBlocks n newf)
runPrg' i (P.Bind f p) =
  let (s1,s2) = split2 i
      (a,prg1) = runPrg' s1 p
      (b,prg2) = runPrg' s2 (f a)
  in (b,prg1 `ProgramSeq` prg2)
runPrg' i (P.Return a) = (a,Skip)
runPrg' i (P.Allocate n t) =
  let nom = "arr" ++ show (supplyValue i)
  in (nom, Allocate nom n t ()) 
runPrg' i (P.Output t) =
  let nom = "output" ++ show (supplyValue i)
  in  (nom, Output nom t)
runPrg' i (P.Sync)     = ((),Synchronize True)

---------------------------------------------------------------------------
-- Printing Programs
---------------------------------------------------------------------------

printPrg :: Show e => Program e -> String 
printPrg Skip = ";\n"
printPrg (Assign name ix e) =
  name ++ "["++ printExp ix ++ "]" ++
  " = " ++ printExp e ++ ";\n"
printPrg (AtomicOp res arr ix op) =
  res ++ " = " ++
  printAtomic op ++ "(" ++ arr ++ "[" ++ printExp ix ++ "]);\n"
printPrg (ForAll n f) =
  "forAll i in [0.."++show n ++"] do\n" ++
  printPrg (f (variable "i")) ++ "\ndone;\n"
printPrg (ForAllBlocks n f) =
  "Blocks i in [0.."++show n ++"] do\n" ++
  printPrg (f (variable "i")) ++ "\ndone;\n"
printPrg (Allocate nom n t e) =
  nom ++ " = malloc(" ++ show n ++ ");\n" ++
  "***" ++ show e ++ "***\n"
printPrg (Output nom t) =
  nom ++ " = newOutput();\n"
printPrg (Synchronize b) = "sync();\n"
printPrg (ProgramSeq p1 p2) =
  printPrg p1 ++ printPrg p2
  

---------------------------------------------------------------------------
-- Analyzing Programs
---------------------------------------------------------------------------
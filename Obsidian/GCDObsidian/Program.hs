{-# LANGUAGE     RankNTypes, 
                 GADTs  #-} 
module Obsidian.GCDObsidian.Program 
       ( Program(..)
       , programThreads
       , (*>*)
       , runPrg
       , printPrg
       )where 

import Data.Word
import Data.Monoid

import Obsidian.GCDObsidian.Exp
import Obsidian.GCDObsidian.Types
import Obsidian.GCDObsidian.Globs
import Obsidian.GCDObsidian.Atomic

-- Package value-supply
import Data.Supply
import System.IO.Unsafe

---------------------------------------------------------------------------
-- 
data Program a where 
  Assign :: Scalar a
            => Name
            -> (Exp Word32)
            -> (Exp a)
            -> Program ()
           
            
  AtomicOp :: Scalar a
              => Name 
              -> Exp Word32
              -> Atomic a
              -> Program (Exp a)

  -- Shouldnt a ForAll have a different result type.
  -- Someting that represents an "array"
  -- Am I mixing up the concepts here ?  (And what are those concepts
  -- that I might be mixing up?) 
  ForAll :: Word32
            -> (Exp Word32 -> Program ())
            -> Program ()


  {-
     I'm not sure about this constructor.
     As I see it programs from which we generate a kernel
     must be wrapped in one of these ForAllBlocks.
     Programs with sequences of 'ForAllBlocks' are problematic.

     Maybe a (ForAllBlocks n f *>* ForAllBlocks m g) Program
     should be split into two kernels. 
  -} 
  ForAllBlocks :: (Exp Word32)
                  -> (Exp Word32 -> Program ()) 
                  -> Program () 

  
  Allocate :: Word32 -> Type -> Program Name


  {- About Output (Creates a named output array). 
     This is similar to Allocate but concerning global arrays.

 
     Since we cannot synchronize writes to a global array inside of an
     kernel, global arrays will only be written as outputs of the kernel
  -} 
  Output   :: Type -> Program Name
  
  
  Sync     :: Program ()

  Return :: a -> Program a
  Bind   :: Program a -> (a -> Program b) -> Program b

---------------------------------------------------------------------------
-- Monad
---------------------------------------------------------------------------
instance Monad Program where
  return = Return
  (>>=) = Bind

---------------------------------------------------------------------------
-- runPrg 
---------------------------------------------------------------------------
runPrg :: Int -> Program a -> (a,Int) 
runPrg i (Return a) = (a,i)
runPrg i (Bind m f) =
  let (a,i') = runPrg i m
  in runPrg i' (f a) 
runPrg i (Sync) = ((),i)
runPrg i (ForAll n ixf) =
  (fst (runPrg i (ixf (variable "tid"))),i) -- FIX THE i
runPrg i (Allocate _ _) = ("new" ++ show i,i+1)
runPrg i (Assign _ _ a) = ((),i) -- Probaby wrong.. 
runPrg i (AtomicOp _ _ _) = (variable ("new"++show i),i+1)

{- 
runPAccm :: Monoid b
            => Int
            -> Program a
            -> (forall a . (Program a -> b))
            -> b 
            -> (a,b,Int)
runPAccm i p@(Return a) f b  = (a, f p `mappend` b,i) 
runPAccm i p@(Bind mf m) f b =
  let (a,acc,i') = runPAccm i m f b 
  in  runPAccm i' (mf a) f acc
runPAccm i p@(Sync) f b = ((),f p `mappend` b, i)
runPAccm i p@(ForAll _ _) f b = ((),f p `mappend` b, i)
runPAccm i p@(Allocate _ _) f b = ("arr" ++ show i,f p `mappend` b, i)
runPAccm i p@(Assign _ _ _)  f b = ((), f p `mappend` b, i)
runPAccm i p@(AtomicOp _ _ _) f b =
  (variable ("new" ++ show i),f p `mappend` b,i+1);
-}
-- took same as precedence as for ++ for *>*


---------------------------------------------------------------------------
-- Sequence programs
---------------------------------------------------------------------------
infixr 5 *>* 

(*>*) :: Program a 
         -> Program b
         -> Program b   
(*>*) p1 p2 = p1 >> p2  
    

---------------------------------------------------------------------------
-- Required threads (reimplement in runPAccm ?)
-- TODO: Maybe just implement this on CodeGen.Program ?? 
---------------------------------------------------------------------------

programThreads :: Program a -> Word32
-- programThreads Skip = 0
programThreads (Sync) = 0 
programThreads (Assign _ _ _) = 1
programThreads (ForAll n f) = n -- inner ForAlls are sequential
programThreads (ForAllBlocks _ f) = programThreads (f (variable "X"))
programThreads (Allocate _ _) = 0 -- programThreads p
programThreads (Bind m f) =
  let (a,_) = runPrg 0 m
  in max (programThreads m) (programThreads (f a))
programThreads (Return a) = 0 
programThreads (AtomicOp _ _ _) = 1

---------------------------------------------------------------------------
-- printPrg
---------------------------------------------------------------------------
printPrg prg = (\(_,x,_) -> x) $ printPrg' 0 prg

printPrg' :: Int -> Program a -> (a,String,Int)  
-- printPrg' i Skip = ((),";\n", i)
printPrg' i (Assign n ix e) =
  ((),n ++ "[" ++ show ix ++ "] = " ++ show e ++ ";\n", i) 
printPrg' i (AtomicOp n ix e) =
  let newname = "r" ++ show i
  in (variable newname,
      newname ++ " = " ++ printAtomic e ++
      "( " ++ n ++ "[" ++ show ix ++ "])\n",i+1)
printPrg' i (Allocate n t) =
  let newname = "arr" ++ show i
  in (newname,newname ++ " = malloc(" ++ show n ++ ");\n",i+1)
printPrg' i (Output t) =
  let newname = "globalOut" ++ show i
  in (newname,newname ++ " = new Global output;\n",i+1)
printPrg' i (ForAll n f) =
  let ((),prg2,i') = printPrg' i (f (variable "i"))
      
  in ( (),  
       "par (i in 0.." ++ show n ++ ")" ++
       "{\n" ++ prg2 ++ "\n}",
       i')
printPrg' i (ForAllBlocks n f) =
  let (d,prg2,i') = printPrg' i (f (variable "BIX"))
  in ((), 
      "blocks (i in 0.." ++ show n ++ ")" ++
      "{\n" ++ prg2 ++ "\n}",
      i')
printPrg' i (Return a) = (a,"MonadReturn;\n",i)
printPrg' i (Bind m f) =
  let (a1, str1,i1) = printPrg' i m
      (a2,str2,i2) = printPrg' i1 (f a1)
  in (a2,str1 ++ str2, i2)
printPrg' i Sync = ((),"Sync;\n",i)




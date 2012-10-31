{-# LANGUAGE     RankNTypes, 
                 GADTs  #-} 
module Obsidian.GCDObsidian.Program 
       (Program(..)
       ,printProgram
       ,programThreads
       ,(*>*)
--       ,targetArray
--       ,targetPair
       ,Atomic(..)
       ,printAtomic
       , runPrg
       , printPrg
--       ,P(..)
        
 --      ,runP
 --      ,(*>>)
 --      ,(*>>>)
 --      ,P(..)
 --      ,runFunc
 --      ,NameSupply
  --     ,unNS
  --     ,runNSDummy
       )where 

import Data.Word
import Data.Monoid

import Obsidian.GCDObsidian.Exp
import Obsidian.GCDObsidian.Types
import Obsidian.GCDObsidian.Globs

-- Package value-supply
import Data.Supply
import System.IO.Unsafe

----------------------------------------------------------------------------
-- 
data Program a where 
  Skip :: Program () 
    
  Assign :: forall a . Scalar a
            => Name
            -> (Exp Word32)
            -> (Exp a)
            -> Program ()
           
            
  AtomicOp :: forall a . Scalar a
              => Name 
              -> Exp Word32
              -> Atomic (Exp a)
              -> Program (Exp a)

  -- Shouldnt a ForAll have a different result type.
  -- Someting that represents an "array"
  -- Am I mixing up the concepts here ?  (And what are those concepts
  -- that I might be mixing up?) 
  ForAll :: Word32
            -> (Exp Word32 -> Program ())
            -> Program ()

  
  ForAllBlocks :: (Exp Word32)
                  -> (Exp Word32 -> Program ()) 
                  -> Program () 
  --        -> Program [a]           

  Allocate :: Word32 -> Type -> Program Name -- Correct type?
  Output   :: Type -> Program Name
  Sync     :: Program ()

  Return :: a -> Program a
  Bind   :: (a -> Program b) -> Program a -> Program b

---------------------------------------------------------------------------
-- Monad
---------------------------------------------------------------------------
instance Monad Program where
  return = Return
  (>>=) = flip Bind


---------------------------------------------------------------------------
-- Continuation 
---------------------------------------------------------------------------
-- Not sure that this bit will help any. (?!)

{- 
data P a =
  P {unP :: forall b . (((a -> Program b) -> Program b))}

instance Monad P where
  return a = P (\k -> k a)
  P f >>= m = P (\k -> f (\a -> unP (m a) k))

runP p = (unP p) (\_ -> Skip)

forAll :: Word32 -> (Exp Word32 -> P ()) -> P ()
forAll n body =
  P $ \k ->
  do
    -- TODO: This must be incorrect..
    --   
    let b = (\i -> unP (body i) (\_ -> Skip)) 
    ForAll n b >> k () 
-} 


---------------------------------------------------------------------------
-- runPrg 
---------------------------------------------------------------------------
runPrg :: Int -> Program a -> (a,Int) 
runPrg i (Return a) = (a,i)
runPrg i (Bind f m) =
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
infixr 5 *>* 

(*>*) :: Program a 
         -> Program b
         -> Program b   
(*>*) p1 p2 = p1 >> p2  
    

---------------------------------------------------------------------------
-- Required threads (reimplement in runPAccm ?) 
---------------------------------------------------------------------------
programThreads :: Program a -> Word32
programThreads Skip = 0
programThreads (Sync) = 0 
programThreads (Assign _ _ _) = 1
programThreads (ForAll n f) = n -- inner ForAlls are sequential
programThreads (Allocate _ _) = 0 -- programThreads p
programThreads (Bind f m) =
  let (a,_) = runPrg 0 m
  in max (programThreads m) (programThreads (f a)) 
  
--  max (programThreads p1) (programThreads p2)
programThreads (AtomicOp _ _ _) = 1



---------------------------------------------------------------------------
-- printProgram 
---------------------------------------------------------------------------
printProgram :: Int -> Program a -> (String,Int)  
printProgram i Skip = (";\n", i)
printProgram i (Assign n ix e) =
  (n ++ "[" ++ show ix ++ "] = " ++ show e ++ ";\n", i) 
printProgram i (AtomicOp n ix e) =
  let newname = "r" ++ show i
  in (newname ++ " = " ++ printAtomic e ++
      "( " ++ n ++ "[" ++ show ix ++ "])\n",i+1)
printProgram i (Allocate n t) =
  let newname = "arr" ++ show i
  in (newname ++ " = malloc(" ++ show n ++ ");\n",i+1)
printProgram i (ForAll n f) =
  let (prg2,i') = printProgram i (f (variable "i"))
  in ("par (i in 0.." ++ show n ++ ")" ++
      "{\n" ++ prg2 ++ "\n}", i')
printProgram i (Return a) = ("MonadReturn;\n",i)
printProgram i (Bind f m) =
  let (str1,i1) = printProgram i m
      (res,_) = runPrg 0 m
      (str2,i2) = printProgram i1 (f res)
  in (str1 ++ str2, i2)
printProgram i Sync = ("Sync;\n",i)

printPrg :: Int -> Program a -> (a,String,Int)  
printPrg i Skip = ((),";\n", i)
printPrg i (Assign n ix e) =
  ((),n ++ "[" ++ show ix ++ "] = " ++ show e ++ ";\n", i) 
printPrg i (AtomicOp n ix e) =
  let newname = "r" ++ show i
  in (variable newname,
      newname ++ " = " ++ printAtomic e ++
      "( " ++ n ++ "[" ++ show ix ++ "])\n",i+1)
printPrg i (Allocate n t) =
  let newname = "arr" ++ show i
  in (newname,newname ++ " = malloc(" ++ show n ++ ");\n",i+1)
printPrg i (Output t) =
  let newname = "globalOut" ++ show i
  in (newname,newname ++ " = new Global output;\n",i+1)
printPrg i (ForAll n f) =
  let ((),prg2,i') = printPrg i (f (variable "i"))
      
  in ( (),  --CHEATING!
       "par (i in 0.." ++ show n ++ ")" ++
       "{\n" ++ prg2 ++ "\n}",
       i')
printPrg i (ForAllBlocks n f) =
  let (d,prg2,i') = printPrg i (f (variable "BIX"))
  in ((), -- CHEATING!
      "blocks (i in 0.." ++ show n ++ ")" ++
      "{\n" ++ prg2 ++ "\n}",
      i')
printPrg i (Return a) = (a,"MonadReturn;\n",i)
printPrg i (Bind f m) =
  let (a1, str1,i1) = printPrg i m
      (a2,str2,i2) = printPrg i1 (f a1)
  in (a2,str1 ++ str2, i2)
printPrg i Sync = ((),"Sync;\n",i)


---------------------------------------------------------------------------
-- Atomic operations 
---------------------------------------------------------------------------
data Atomic a where
  AtomicInc :: Atomic (Data Int)

printAtomic AtomicInc = "atomicInc"





---------------------------------------------------------------------------
-- Tests
---------------------------------------------------------------------------

testPrg1 :: Program ()
testPrg1 =
  do
    somewhere <- Allocate 100 Int 
    ForAll 100
           (\i -> Assign somewhere i (variable "hej" :: Exp Int))

testPrg2 :: Program ()
testPrg2 =
  do
    somewhere <- Allocate 175 Int 
    ForAll 175
           (\i -> Assign somewhere i (variable "a" :: Exp Int))

testPrg3 :: Program ()
testPrg3 = testPrg1 >> Sync >> testPrg2 

---------------------------------------------------------------------------
-- Small push/pull array test
---------------------------------------------------------------------------
data PushArray a =
  PA (forall b . (((Exp Word32, a) -> Program ()) -> Program ()))
  
data PullArray a = PU Word32 (Exp Word32 -> a) 

myPullArray :: PullArray (Exp Int) 
myPullArray = PU 100 (\ix -> (index "input0" ix) )

push1 :: PullArray (Exp Int) -> PushArray (Exp Int)
push1 (PU n ixf) =
  PA (\k -> ForAll n (\i -> k (i,ixf i)))


-- Like Sync. 
force :: PushArray (Exp Int) -> Program (PullArray (Exp Int))
force (PA cont) = -- I Forgot to add lengths to PAs 
  do
    imm <- Allocate 100 {- made up -} Int
    cont (target imm)
    return (PU 100 (\ ix -> index imm ix)) 
    
    
   
target :: Name -> (Exp Word32, Exp Int) -> Program ()
target name (ix,x) = Assign name ix x 

testPrg4 = force (push1 myPullArray)

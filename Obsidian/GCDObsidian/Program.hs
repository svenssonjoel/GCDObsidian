{-# LANGUAGE     RankNTypes, 
                 GADTs  #-} 
module Obsidian.GCDObsidian.Program 
       (Program(..)
       ,printProgram
       ,programThreads
       ,(*>*)
       ,targetArray
       ,targetPair
       ,Atomic(..)
       ,printAtomic

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
-- TODO: Data a and Exp a is the same. (look at this!) 
data Program extra res where 
  Skip :: Program extra () 
    
  Assign :: forall extra a . Scalar a
            => Name
            -> (Exp Word32)
            -> (Exp a)
            -> Program extra ()
           
            
  AtomicOp :: forall extra a . Scalar a
              => Name 
              -> Exp Word32
              -> Atomic (Exp a)
              -> Program extra (Exp a)

  -- Shouldnt a ForAll have a different result type.
  -- Someting that represents an "array" 
  ForAll :: Word32
            -> (Exp Word32 -> Program extra ())
            -> Program extra ()

  --        -> Program extra [a]           

  Allocate :: Word32 -> Type -> extra -> Program extra Name -- Correct type?
  Sync     :: Program extra ()

  Return :: a -> Program extra a
  Bind   :: (a -> Program extra b) -> Program extra a -> Program extra b

---------------------------------------------------------------------------
-- Tests
---------------------------------------------------------------------------

testPrg1 :: Program () () 
testPrg1 =
  do
    somewhere <- Allocate 100 Int () 
    ForAll 100
           (\i -> Assign somewhere i (variable "hej" :: Exp Int))

testPrg2 :: Program () ()
testPrg2 =
  do
    somewhere <- Allocate 175 Int () 
    ForAll 175
           (\i -> Assign somewhere i (variable "a" :: Exp Int))

testPrg3 :: Program () ()
testPrg3 = testPrg1 >> Sync >> testPrg2 

---------------------------------------------------------------------------
-- Small push/pull array test
---------------------------------------------------------------------------
data PushArray a =
  PA (forall b. ((Exp Word32, a) -> Program () ()) -> Program () ())
data PullArray a = PU Word32 (Exp Word32 -> a) 

myPullArray :: PullArray (Exp Int) 
myPullArray = PU 100 (\ix -> (index "input0" ix) )

push1 :: PullArray (Exp Int) -> PushArray (Exp Int)
push1 (PU n ixf) =
  PA (\k -> do
         ForAll n
           (\i -> k (i,ixf i)))

writeP :: PushArray (Exp Int) -> Program () ()
writeP (PA p) =
  do
    name <-  Allocate 100 Int ()
    p (target name) 

target :: Name -> (Exp Word32, Exp Int) -> Program () ()
target name (ix,x) = Assign name ix x 

testPrg4 = writeP (push1 myPullArray)
---------------------------------------------------------------------------
-- Monad
---------------------------------------------------------------------------

instance Monad (Program extra) where
  return = Return
  (>>=) = flip Bind

---------------------------------------------------------------------------
-- runP 
---------------------------------------------------------------------------
runP :: Int -> Program extra a -> (a,Int) 
runP i (Return a) = (a,i)
runP i (Bind f m) =
  let (a,i') = runP i m
  in runP i' (f a) 
runP i (Sync) = ((),i)
runP i (ForAll _ _ ) = ((),i)
runP i (Allocate _ _ _) = ("new" ++ show i,i+1)
runP i (Assign _ _ _) = ((),i)
runP i (AtomicOp _ _ _) = (variable ("new"++show i),i+1)

runPAccm :: Monoid b
            => Int
            -> Program extra a
            -> (forall a . (Program extra a -> b))
            -> b 
            -> (a,b,Int)
runPAccm i p@(Return a) f b  = (a, f p `mappend` b,i) 
runPAccm i p@(Bind mf m) f b =
  let (a,acc,i') = runPAccm i m f b 
  in  runPAccm i' (mf a) f acc
runPAccm i p@(Sync) f b = ((),f p `mappend` b, i)
runPAccm i p@(ForAll _ _) f b = ((),f p `mappend` b, i)
runPAccm i p@(Allocate _ _ _) f b = ("arr" ++ show i,f p `mappend` b, i)
runPAccm i p@(Assign _ _ _)  f b = ((), f p `mappend` b, i)
runPAccm i p@(AtomicOp _ _ _) f b =
  (variable ("new" ++ show i),f p `mappend` b,i+1);

-- took same as precedence as for ++ for *>*
infixr 5 *>* 

(*>*) :: Program extra a 
         -> Program extra b
         -> Program extra b   
(*>*) p1 p2 = p1 >> p2  
    

---------------------------------------------------------------------------
-- Required threads (reimplement in runPAccm ?) 
---------------------------------------------------------------------------
programThreads :: Program extra a -> Word32
programThreads Skip = 0
programThreads (Sync) = 0 
programThreads (Assign _ _ _) = 1
programThreads (ForAll n f) = n -- inner ForAlls are sequential
programThreads (Allocate _ _ _) = 0 -- programThreads p
programThreads (Bind f m) =
  let (a,_) = runP 0 m
  in max (programThreads m) (programThreads (f a)) 
  
--  max (programThreads p1) (programThreads p2)
programThreads (AtomicOp _ _ _) = 1



---------------------------------------------------------------------------
-- printProgram 
---------------------------------------------------------------------------
printProgram :: Int -> Program () a -> (String,Int)  
printProgram i Skip = (";\n", i)
printProgram i (Assign n ix e) =
  (n ++ "[" ++ show ix ++ "] = " ++ show e ++ ";\n", i) 
printProgram i (AtomicOp n ix e) =
  let newname = "r" ++ show i
  in (newname ++ " = " ++ printAtomic e ++
      "( " ++ n ++ "[" ++ show ix ++ "])\n",i+1)
printProgram i (Allocate n t extra) =
  let newname = "arr" ++ show i
  in (newname ++ " = malloc(" ++ show n ++ ");\n",i+1)
printProgram i (ForAll n f) =
  let (prg2,i') = printProgram i (f (variable "i"))
  in ("par (i in 0.." ++ show n ++ ")" ++
      "{\n" ++ prg2 ++ "\n}", i')
printProgram i (Return a) = ("MonadReturn;\n",i)
printProgram i (Bind f m) =
  let (str1,i1) = printProgram i m
      (res,_) = runP 0 m
      (str2,i2) = printProgram i1 (f res)
  in (str1 ++ str2, i2)
printProgram i Sync = ("Sync;\n",i)

printPrg :: Int -> Program () a -> (a,String,Int)  
printPrg i Skip = ((),";\n", i)
printPrg i (Assign n ix e) =
  ((),n ++ "[" ++ show ix ++ "] = " ++ show e ++ ";\n", i) 
printPrg i (AtomicOp n ix e) =
  let newname = "r" ++ show i
  in (variable newname,
      newname ++ " = " ++ printAtomic e ++
      "( " ++ n ++ "[" ++ show ix ++ "])\n",i+1)
printPrg i (Allocate n t extra) =
  let newname = "arr" ++ show i
  in (newname,newname ++ " = malloc(" ++ show n ++ ");\n",i+1)
printPrg i (ForAll n f) =
  let (_,prg2,i') = printPrg i (f (variable "i"))
  in ( (),
       "par (i in 0.." ++ show n ++ ")" ++
       "{\n" ++ prg2 ++ "\n}",
       i')
printPrg i (Return a) = (a,"MonadReturn;\n",i)
printPrg i (Bind f m) =
  let (a1, str1,i1) = printPrg i m
      -- (res,_) = runP 0 m
      (a2,str2,i2) = printPrg i1 (f a1)
  in (a2,str1 ++ str2, i2)
printPrg i Sync = ((),"Sync;\n",i)


---------------------------------------------------------------------------
--
---------------------------------------------------------------------------

--targetArray :: Scalar a => Name -> (Exp Word32,Exp a) -> Program () ()
--targetArray n (i,a) = Assign n i a 


--targetPair :: (Scalar a, Scalar b) => Name -> Name -> (Exp Word32,(Exp a,Exp b)) -> Program () ()
--targetPair n1 n2  (i,(a,b)) = Assign n1 i a *>*
--                              Assign n2 i b

-- Atomic operations

data Atomic a where
  AtomicInc :: Atomic (Data Int)

printAtomic AtomicInc = "atomicInc"


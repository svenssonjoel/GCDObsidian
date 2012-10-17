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

-- Package value-supply
import Data.Supply
import System.IO.Unsafe

----------------------------------------------------------------------------
-- 
-- TODO: Data a and Exp a is the same. (look at this!) 
data Program extra
  = Skip
    
  | forall a. Scalar a => Assign Name (Data Word32) (Data a) 
-- Note: Writing of a scalar value into an array location.     
  | ForAll (Data Word32 -> (Program extra)) Word32   

-- Could this be improved ? 
  | ForAllGlobal (Data Word32 -> (Program extra)) (Exp Word32)
    
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
printProgram (AtomicOp d n i e) = d ++ " = " ++ printAtomic e ++
                                     "(" ++ n ++ "[" ++ show i ++ "])\n"

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


----------------------------------------------------------------------
-- A monadic interface to Program

-- I've set extra = (). I hope that's ok, but it's not a big thing. It's eays
-- to change.

-- The extra Int argument is for generating fresh variable names

data P a = P { unP :: (a -> NameSupply (Program ())) -> NameSupply (Program ())}

instance Monad P where
  return a  = P (\k -> k a)
  P f >>= m = P (\k -> f (\a -> unP (m a) k))

runP :: P a -> Program ()
runP (P m) = unNS (m (\_ -> return Skip)) (unsafePerformIO (newEnumSupply))

(*>>) :: Program () -> NameSupply (Program ()) -> NameSupply (Program ())
p *>> m = do p' <- m
             return (p *>* p')

skip :: P ()
skip = P (\k -> Skip *>> k ())

assign :: Scalar a => Name -> Data Word32 -> Data a -> P ()
assign name ix e = P (\k -> Assign name ix e *>> k ())

forAll :: Word32 -> (Data Word32 -> P ()) -> P ()
forAll l body = P (\k -> do b <- runFunc (\i -> unP (body i) (\_ -> return Skip))
                            ForAll b l *>> k ())

forAllGlobal :: Data Word32 -> (Data Word32 -> P ()) -> P ()
forAllGlobal l body = P (\k -> do b <- runFunc (\i -> unP (body i) (\_ -> return Skip))
                                  ForAllGlobal b l *>> k ())

allocate :: Name -> Word32 -> Type -> P ()
allocate name ix ty = P (\k -> Allocate name ix ty () *>> k ())

synchronize :: P ()
synchronize = P (\k -> Synchronize True *>> k ())
-- TODO: what should the boolean parameter be?

atomicOp :: Scalar a => Name -> Data Word32 -> Atomic (Data a) -> P (Data a)
atomicOp name ix atomic = P (\k -> do dest <- newName "a"
                                      AtomicOp dest name ix atomic *>> k (Index (dest,[])))

-- A monad to generate fresh names for the variables assigned in the atomic
-- operations
data NameSupply a = NS { unNS :: Supply Int -> a }

instance Monad NameSupply where
  return a = NS $ \_ -> a
  NS f >>= m = NS $ \s ->
    let (s1,s2) = split2 s in
    unNS (m (f s1)) s2

newName :: String -> NameSupply String
newName v = NS $ \s -> v ++ show (supplyValue s)

runFunc :: (a -> NameSupply b) -> NameSupply (a -> b)
runFunc f = NS $ \s -> \a -> unNS (f a) s
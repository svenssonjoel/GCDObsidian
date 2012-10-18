{-# LANGUAGE     RankNTypes, 
                 GADTs,
                 GeneralizedNewtypeDeriving #-} 
module Obsidian.GCDObsidian.Program 
       (Program(..)
       ,printProgram
       ,programThreads
       ,(*>*)
       ,targetArray
       ,targetPair
       ,Atomic(..)
       ,printAtomic

        --- Experimental
       ,runFunc
       ,P(..)
       ,(*>>>)
       ,newName
       )where 

import Data.Word

import Obsidian.GCDObsidian.Exp
import Obsidian.GCDObsidian.Types
import Obsidian.GCDObsidian.Globs


--import Control.Monad.State

import Data.Supply
import System.IO.Unsafe
----------------------------------------------------------------------------
-- 
data Program extra 
  = Skip
  | forall a. Scalar a => Assign Name (Exp Word32) (Exp a) 

  | ForAll Word32 (Exp Word32 -> (Program extra))    
  | ForAllGlobal Word32
                 (Exp Word32 -> Exp Word32 -> (Program extra)) 

  | Allocate Name Word32 Type extra

  | Synchronize Bool
  | ProgramSeq (Program extra) 
               (Program extra)

-- Be careful so that a conditional program does not contain Synchronize  
  | Cond    (Exp Bool) (Program extra)
    
  | forall a. Scalar a => AtomicOp Name Name (Exp Word32) (Atomic (Exp a))

-- took same as precedence as for ++ for *>*
infixr 5 *>* 

(*>*) :: Program extra 
         -> Program extra 
         -> Program extra    
(*>*) = ProgramSeq 

--TODO: There should be a threadsPerBlock function instead. 
programThreads :: Program extra -> Word32
programThreads Skip = 0
programThreads (Synchronize _) = 0 
programThreads (Assign _ _ _) = 1
programThreads (ForAll n f) = n -- inner ForAlls are sequential
programThreads (Allocate _ _ _ _) = 0 -- programThreads p 
-- programThreads (Cond b p ) = programThreads p
programThreads (p1 `ProgramSeq` p2) = max (programThreads p1) (programThreads p2)
programThreads (AtomicOp _ _ _ _) = 1

---------------------------------------------------------------------------
-- reqBlockSize / reqGridSize 
---------------------------------------------------------------------------
-- gives the number of threads needed.
reqBlockSize :: Program extra -> Word32
reqBlockSize (ForAll n _) = n
reqBlockSize (ForAllGlobal n _) = n
reqBlockSize (p1 `ProgramSeq`  p2) = max (reqBlockSize p1)
                                         (reqBlockSize p2)
reqBlockSize _ = 0

-- gives the number of blocks needed.
-- No! grids are dynamic
--reqGridSize :: Program extra -> Word32
--reqGridSize (ForAllGlobal g _ _) = g
--reqGridSize (p1 `ProgramSeq` p2) = max (reqGridSize p1)
--                                       (reqGridSize p2)
--reqGridSize _ = 0 -- ??? 


---------------------------------------------------------------------------
-- Print a program
---------------------------------------------------------------------------
printProgram :: Show extra => Program extra -> String 
printProgram Skip = ";" 
printProgram (Synchronize b) = "Sync " ++ show b ++ "\n" 
printProgram (Assign n t e) = n ++ "[" ++ show t ++ "]" ++
                                    " = " ++ show e ++ ";\n"  
printProgram (ForAll n f)   = "par i " ++ show n ++
                              " {\n" ++
                              printProgram (f (variable "i")) ++
                              "\n}"
printProgram (ForAllGlobal bn f) = "parG (bi in all blocks)  "  ++ " ti " ++ show bn
                                      ++ "{\n" ++
                                      printProgram (f (variable "bi") (variable "ti"))
                                      ++ "\n}"
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
  AtomicInc :: Atomic (Exp Int)

printAtomic AtomicInc = "atomicInc"

---------------------------------------------------------------------------
-- P Monad 
---------------------------------------------------------------------------
data P a = P {unP :: (a -> NameSupply (Program ())) -> NameSupply (Program ())}

instance Monad P where
  return a = P (\k -> k a)
  P f  >>= m = P (\k -> f (\a -> unP (m a) k))


runP :: P a -> Program ()
runP (P m) = runNS (m (\_ -> return Skip)) 


(*>>) :: Program () -> NameSupply (Program ())  -> NameSupply (Program ()) 
p *>> m =
  do 
    p' <- m
    return (p *>* p')

-- Sequence NameSupply (Program ())  
(*>>>) :: NameSupply (Program ()) 
          -> NameSupply (Program ()) 
          -> NameSupply (Program ())
m1 *>>> m2 =
  do
    p1 <- m1
    p2 <- m2
    return (p1 *>* p2) 
  

---------------------------------------------------------------------------
-- Wrappers on Program constructors.  
---------------------------------------------------------------------------
skip :: P ()
skip = P (\k -> Skip *>> k ())

assign :: Scalar a => Name -> Exp Word32 -> Exp a  -> P ()
assign name ix e = P (\k -> Assign name ix e *>> k ())


forAll :: Word32 -> (Exp Word32 -> P a) -> P a
forAll l body = P (\k ->
                    do
                      b <- runFunc (\i -> unP (body i) k)
                      return (ForAll l b)) 
               
---------------------------------------------------------------------------
--  New NameSupply monad. 
---------------------------------------------------------------------------
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


runNS :: NameSupply a -> a 
runNS (NS ns) = ns (unsafePerformIO newEnumSupply)
  -- unNS (m (\_ -> return Skip)) (unsafePerformIO (newEnumSupply))


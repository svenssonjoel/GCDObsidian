
module Obsidian.GCDObsidian.Kernel where 

import Control.Monad  
import Control.Monad.State
import Control.Monad.Writer
import Data.Monoid
import Data.Word
import Data.List 
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Map as Map
import Prelude hiding (elem)

import Obsidian.GCDObsidian.Exp
import Obsidian.GCDObsidian.Array
-- import Obsidian.GCDObsidian.Memory
import Obsidian.GCDObsidian.Globs
import Obsidian.GCDObsidian.Program

------------------------------------------------------------------------------  
-- threadId, here or elsewhere ?

tid :: Exp Word32
tid = variable "tid"
      
type NumThreads = Word32      
      
---------------------------------------------------------------------------
-- Kernels
---------------------------------------------------------------------------

instance Monoid (Program extra) where 
  mempty = Skip 
  mappend Skip a = a 
  mappend a Skip = a 
  mappend (p1 `ProgramSeq` p2) p3 = p1 `ProgramSeq` (mappend p2 p3) 
  mappend p1 (p2 `ProgramSeq` p3) = (p1 ` ProgramSeq` p2) `mappend` p3  
  mappend p1 p2 = p1 `ProgramSeq` p2
  
  
type Kernel a = StateT Integer (Writer (Program ())) a

runKernel :: Kernel k -> ((k,Integer),Program ())
runKernel k = runWriter $ runStateT k 0

evalKernel :: Kernel k -> k
evalKernel k = fst $ runWriter $ evalStateT k 0

getKernelProgram :: Kernel k -> Program ()
getKernelProgram k = execWriter $ runStateT k 0

newArray :: Kernel Name 
newArray  = do
  i <- get
  let newName = "arr" ++ show i 
  put (i+1)
  return newName


(->-) :: (a -> Kernel b) -> (b -> Kernel c) -> (a -> Kernel c) 
(->-) = (>=>) 

pure f a = return (f a) 

----------------------------------------------------------------------------
-- Figure out how many threads a piece of Code requires
threadsNeeded :: Program e -> Word32
threadsNeeded = programThreads 

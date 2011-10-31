module ArrowTest where 


import Obsidian.GCDObsidian
import Obsidian.GCDObsidian.AddOn.ArrowObsidian 
import Obsidian.GCDObsidian.CodeGen.CUDA as CUDA 

import Control.Category
import Prelude hiding ((.),id)


-- Most simple test
test1 :: Array (Exp Int) -> Kernel (Array (Exp Int)) 
test1 = runArrow id

getTest1 = putStrLn$ CUDA.genKernel "test1" test1 (namedArray "apa" 32)


-- Test that uses things from Obsidian standard library
-- (these are reusable in the ArrowAddOn
test2 :: Array (Exp Int) -> Kernel (Array (Exp Int)) 
test2 = runArrow$ Pure rev . Pure (fmap (+1))  

getTest2 = putStrLn$ CUDA.genKernel "test2" test2 (namedArray "apa" 32)


-- Test that uses both AddOn and base Obsidian
test3 :: Array (Exp Int) -> Kernel (Array (Exp Int))
test3  = pure (fmap (+1)) ->- (runArrow (Pure (fmap (+1))))   
  
getTest3 = putStrLn$ CUDA.genKernel "test3" test3 (namedArray "apa" 32)


-- Same as above but with a sync
test4 :: Array (Exp Int) -> Kernel (Array (Exp Int))
test4  = pure (fmap (+1)) ->- sync ->- (runArrow (Pure (fmap (+1))))   
  
getTest4 = putStrLn$ CUDA.genKernel "test4" test4 (namedArray "apa" 32)


test5 :: Array (Exp Int) -> Kernel (Array (Exp Int)) 
test5 = pure rev ->- (runArrow (two ((Pure rev) . aSync . (Pure rev))))
getTest5 = putStrLn$ CUDA.genKernel "test5" test5 (namedArray "apa" 32)

{- These tests show the need for a few things. 
   #1 overloaded pure
   #2 overloaded sync ? 
   #3 use . for composition in base Obsidian also for uniform look 
-}
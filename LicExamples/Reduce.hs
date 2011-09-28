{-# LANGUAGE FlexibleContexts #-}
module Reduce where 

import Obsidian.GCDObsidian

import qualified Obsidian.GCDObsidian.CodeGen.CUDA as CUDA

----------------------------------------------------------------------------
-- DONE: This is too limited (Scalar a).
--       Comes from the sync. Figure out how to generalize
-- TODO: The issue above again raises a questionmark about the "Tuples" 
--       in the Exp type
--
-- DONE: Shows an error in the code generation. 
--       The writing of the result is not done by just a single thread
--       as it should!

reduce :: Syncable Array a => 
          Int -> (a -> a -> a) -> Array a -> Kernel (Array a)  
reduce 0 f = pure id 
reduce n f = pure op ->- sync ->- reduce (n-1) f
  where 
    op = fmap (uncurry f) . pair

getReduce = putStrLn$ CUDA.genKernel "reduce" (reduce 3 (+)) (namedArray "apa" 8 :: Array (Exp Int))

  
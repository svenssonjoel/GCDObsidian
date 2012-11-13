

module Obsidian.GCDObsidian.CodeGen.CUDA.Compile where

import Obsidian.GCDObsidian.CodeGen.CUDA 

-- This module should interact with the nvcc compiler
-- via the shell (I imagine)


import System.Process

-- TODO: This function should take some more parameters.
--       The architecture to compile for.. 
storeAndCompile :: FilePath -> String -> IO FilePath
storeAndCompile fp code =
  do
    writeFile fp code
    
    let nfp = fp ++  ".cubin"

    (_,_,_,pid) <-
      createProcess (shell ("nvcc -cubin -o " ++ nfp ++ " " ++ fp))
    exitCode <- waitForProcess pid
    return nfp


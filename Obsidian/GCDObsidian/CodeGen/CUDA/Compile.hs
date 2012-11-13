

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
    -- Somehow ensure that the file has been written successfully
    -- before trying to compile it. (problem on the fast machine) 
    createProcess (shell ("nvcc -arch=sm_30 -cubin -o " ++ nfp ++ " " ++ fp))
    return nfp


module Obsidian.GCDObsidian.Types where 


data Type = Bool | Int | Word8 | Word16 | Word32 | Word64 
          | Float | Double                     
                    
          -- Used by CUDA, C And OpenCL generators          
          | Pointer Type   -- C thing 
          | Global Type    -- OpenCL thing
          | Local Type     -- OpenCL thing
          deriving Show
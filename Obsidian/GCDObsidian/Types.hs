module Obsidian.GCDObsidian.Types where 


---------------------------------------------------------------------------
data Float2 = F2 
data Int2   = I2 
data Float4 = F4 
data Int4   = I4 

data Type 
  -- The allowed scalar types
  = Bool | Int | Word8 | Word16 | Word32 | Word64 
  | Float | Double                     
            
  -- 2 element vectors *CUDA Specific ?    
  | Float2
  | Int2   
            
  -- 4 element vectors  *CUDA Specific ?
  | Float4 
  | Int4   
    
    -- Used by CUDA, C And OpenCL generators          
  | Pointer Type   -- C thing 
  | Global Type    -- OpenCL thing
  | Local Type     -- OpenCL thing
  deriving (Eq, Show)
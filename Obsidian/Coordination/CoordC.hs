module Obsidian.Coordination.CoordC where 




import Obsidian.GCDObsidian.Exp 
import Obsidian.GCDObsidian.Types 



import Data.Word

import Data.Monoid 

data LaunchConfig = LaunchConfig {launchBlocks  :: Int, 
                                  launchThreads :: Int, 
                                  launchShared  :: Int}
                    deriving (Eq, Show)

data CVar = CVar String Type 
          deriving (Eq, Show)

type IOS = [CVar] 


-- TODO: print as CUDA and OpenCL C code. 
--      
data CoordC 
  = Skip 
  | Malloc CVar (Exp Word32)
  | Launch String LaunchConfig IOS IOS 
  | MemCpy CVar CVar (Exp Word32)
  | Free   CVar
  | Seq    CoordC CoordC 
    deriving (Eq,Show) 


--  CoordFunction                  Name   ins outs  body 
data CoordFunction = CoordFunction String IOS IOS   CoordC 

instance Monoid CoordC where 
  mempty = Skip
  mappend Skip b = b 
  mappend a Skip = a 
  mappend a b = a `Seq` b
         
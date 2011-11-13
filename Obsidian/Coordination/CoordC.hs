module Obsidian.Coordination.CoordC where 




import Obsidian.GCDObsidian.Exp 
import Obsidian.GCDObsidian.Types 



import Data.Word



data LaunchConfig = LaunchConfig {launchBlocks  :: Int, 
                                  launchThreads :: Int, 
                                  launchShared  :: Int}

data CVar = CVar String Type 

type IOS = [CVar] 


-- TODO: print as CUDA and OpenCL C code. 
--      
data CoordC 
  = Malloc CVar Int
  | Launch String LaunchConfig IOS IOS 
  | MemCpy CVar CVar Int 
  | Free   CVar
  | Seq    CoordC CoordC 


--  CoordFunction                  Name   ins outs  body 
data CoordFunction = CoordFunction String IOS IOS   CoordC 

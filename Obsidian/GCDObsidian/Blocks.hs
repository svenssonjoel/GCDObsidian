
module Obsidian.GCDObsidian.Blocks where 

import Obsidian.GCDObsidian.Exp
import Obsidian.GCDObsidian.Globs
import Obsidian.GCDObsidian.Program
import Obsidian.GCDObsidian.Array 

import Data.Word



data Blocks a = Blocks (Exp Word32)   -- number of 
                       -- (Word32)       -- size of each 
                       (Exp Word32 -> a)

namedGlobal name bn bs = Blocks bn {- bs -} 
                         (\bix -> (mkPullArray bs
                                   (\ix -> index name (bix * (fromIntegral bs) + ix)))) 


{-
similar to a pull array but represents the division
of work over blocks. 


-} 

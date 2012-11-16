{- Joel Svensson 2012 -} 
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

(!|) (Blocks bn blkf) bix = blkf bix 

{-
similar to a pull array but represents the division
of work over blocks. 


-} 


---------------------------------------------------------------------------
-- map over blocks. Normal Functor instance is fine! 
---------------------------------------------------------------------------
instance Functor Blocks where
  fmap f (Blocks nb bxf) = Blocks nb (\bix -> (f (bxf bix)))

---------------------------------------------------------------------------
-- zipWith. (Normal zipWith)
---------------------------------------------------------------------------
zipBlocksWith :: (a -> b -> c)
                 -> Blocks a -> Blocks b -> Blocks c
zipBlocksWith f (Blocks nb1 bxf1)
                (Blocks nb2 bxf2) =
  Blocks (min nb1 nb2) (\bix -> f (bxf1 bix) (bxf2 bix))

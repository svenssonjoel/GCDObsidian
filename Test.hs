

import Obsidian.GCDObsidian.Exp 
import Obsidian.GCDObsidian.Program 
import Obsidian.GCDObsidian.Array
import Obsidian.GCDObsidian.Blocks

import Data.Word

data PushB a = Pb (Exp Word32)
                  ((Exp Word32 -> a -> Exp Word32 -> Program ()) -> (Exp Word32 -> Program ()))

-- PushB a = Pb ((a -> Exp Word32 -> Exp Word32 -> Program ()) ->
--                    (Exp Word32 -> Exp Word32 -> Program ()))

-- PushP r a = PP ((Exp Word32 -> a -> r) -> r)) 

{-
map :: (Array Pull a -> PushP (P()) a) ->
       Block (Array Pull a) -> BlockP PushP a

type BlockP p a = p (Exp Word32 -> P ()) a
-} 


histogram :: Blocks (Array Pull (Exp Word32))
             -> PushB (Exp Word32)
histogram (Blocks nb blkf) =
  Pb nb (\wf bix ->
          let arr = blkf bix
              blkSize = len arr
          in ForAll blkSize $ \i -> let ix' = arr ! i
                                        blk = ix' `div` fromIntegral blkSize
                                        ix  = ix' `mod` fromIntegral blkSize  
                                    in  wf ix 1 blk)


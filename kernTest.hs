
import Obsidian.GCDObsidian.Kernel
import Obsidian.GCDObsidian.Library
import Obsidian.GCDObsidian.Array 
import Obsidian.GCDObsidian.Exp
import Obsidian.GCDObsidian.Sync


test1 :: Array (Exp Int) -> Kernel (Array (Exp Int))
test1 = pure (fmap (+1))


test2 :: Array (Exp Int) -> Kernel (Array (Exp Int)) 
test2 inp = do 
  a1 <- pSyncArray inp
  a2 <- pSyncArray a1 
  a3 <- pSyncArray a2 
  a4 <- pSyncArray a3
  a5 <- pSyncArray a1
  return a3
  
  


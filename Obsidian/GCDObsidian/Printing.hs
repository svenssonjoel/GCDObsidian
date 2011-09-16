{-# LANGUAGE FlexibleContexts #-}

module Obsidian.GCDObsidian.Printing  where 



import  Obsidian.GCDObsidian.Exp 
import  Obsidian.GCDObsidian.Kernel 
import  Obsidian.GCDObsidian.Array
 


------------------------------------------------------------------------------
-- Print Code
printCode :: Show a => Code a -> IO ()
printCode c = putStrLn$ codeToString c


codeToString :: Show extra => Code extra -> String
codeToString Skip = "SKIP\n" 
codeToString (su `Seq` code) = syncunitToString su ++ codeToString code

syncunitToString (SyncUnit nt sl) = 
  "SYNCUNIT " ++ show nt ++ "{ \n" ++ 
  storelistToString sl ++ "\n}\n"
  
storelistToString StoreListNil = ""
storelistToString (StoreListCons store sl) = 
  storeToString store ++ "\n" ++ storelistToString sl


storeToString :: Show extra => Store a extra  -> String 
storeToString (Store name size ws) = name++ "[" ++ show size ++ "]\n" ++ 
                                     concatMap writeToString ws ++ "}\n"  


writeToString :: Show extra => Write a extra -> String 
writeToString (Write targf ll e) = 
  "whereStore: \\ix -> " ++ printExp (targf (variable "ix")) ++ "\n" ++
  "ToCompute: " ++  printExp exp ++ "\n" ++ 
  "extra: " ++ show e ++ "\n"
  where 
    exp = ll `llIndex` tid


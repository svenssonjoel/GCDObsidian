{-# LANGUAGE FlexibleContexts #-}

module Obsidian.GCDObsidian.Printing  where 

import Obsidian.GCDObsidian.Exp 
import Obsidian.GCDObsidian.Kernel 
import Obsidian.GCDObsidian.Array
import Obsidian.GCDObsidian.Program  


------------------------------------------------------------------------------
-- Print Code
printCode :: Show a => Code a -> IO ()
printCode c = putStrLn$ codeToString c


codeToString :: Show extra => Code extra -> String
codeToString Skip = "SKIP\n" 
codeToString (su `Seq` code) = syncunitToString su ++ codeToString code

syncunitToString (SyncUnit nt ps extra) = 
  "SYNCUNIT " ++ show nt ++ "[" ++ show extra ++ "]" ++ "{ \n" ++ 
  programsToString ps ++ "\n}\n"
  
programsToString [] = ""  
programsToString (p:ps) = programToString p ++ programsToString ps

programToString = printProgram
  
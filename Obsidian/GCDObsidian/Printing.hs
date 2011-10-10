{-# LANGUAGE FlexibleContexts #-}

module Obsidian.GCDObsidian.Printing  where 

import Obsidian.GCDObsidian.Exp 
import Obsidian.GCDObsidian.Kernel 
import Obsidian.GCDObsidian.Array
import Obsidian.GCDObsidian.Program  


------------------------------------------------------------------------------
-- Print Code
--printCode :: Show a => Code a -> IO ()
--printCode c = putStrLn$ codeToString c


--codeToString :: Show extra => Code extra -> String
--codeToString Skip = "SKIP\n" 
--codeToString (su `Seq` code) = syncunitToString su ++ codeToString code

--syncunitToString (SyncUnit nt p extra) = 
--  "SYNCUNIT " ++ show nt ++ "[" ++ show extra ++ "]" ++ "{ \n" ++ 
--  programToString p ++ "\n}\n"
  
{-
programsToString [] = ""  
programsToString (p:ps) = programToString p ++ programsToString ps
-}
printCode :: Show a => Program a -> IO () 
printCode c = putStrLn$ printProgram c

programToString :: Show a => Program a -> String 
programToString = printProgram
  
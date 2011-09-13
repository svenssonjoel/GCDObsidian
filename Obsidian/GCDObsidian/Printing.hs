
module Obsidian.GCDObsidian.Printing  where 



import  Obsidian.GCDObsidian.Exp 
import  Obsidian.GCDObsidian.Kernel 
import  Obsidian.GCDObsidian.Array
 


------------------------------------------------------------------------------
-- Print Code
printCode :: Show a => Code a -> IO ()
printCode c = putStrLn "hello world"  -- putStrLn$ codeToStr


{-
codeToString :: Show a => Code a -> String
codeToString Skip = "\n" 
codeToString (Seq s c) = storeToString s ++ codeToString c

storeToString :: Show a => Store a -> String 
storeToString (Store nt ws) = "Using " ++ show nt ++ " threads compute {\n" ++
                              concatMap writeToString ws ++ "}\n" 

writeToString :: Show a => Write a -> String 
writeToString (Write n ll e) = 
  printExp (n tid) ++ " = " ++ printExp exp ++ "\n" ++ 
  show e ++ "\n"
  where 
    exp = ll ! tid
-}
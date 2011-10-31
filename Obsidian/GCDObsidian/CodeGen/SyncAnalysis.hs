module Obsidian.GCDObsidian.CodeGen.SyncAnalysis where




import Obsidian.GCDObsidian.Kernel
import Obsidian.GCDObsidian.Program
import Obsidian.GCDObsidian.Exp 
import Obsidian.GCDObsidian.Globs

import Data.Word
import Data.List
import qualified Data.Map as Map

-- TODO: document this file 

-- TEMap is an Index->Thread map
type TEMap = Map.Map Word32 Word32 

-- SAMap is a name->TEMap map 
type SAMap = Map.Map Name TEMap

warpSize = 32



{- 
  input program should have unique names for all intermediate arrays
-} 
syncAnalysis :: Show a => Program a -> SAMap -> (SAMap,Program a) 
syncAnalysis Skip sam = (sam,Skip) 
syncAnalysis (Synchronize b) sam = (sam,Synchronize False) 
syncAnalysis f@(ForAll _ _) sam = analyseForAll f sam 
syncAnalysis a@(Allocate nom n t e) sam = (sam,a) 
syncAnalysis (ProgramSeq prg1 prg2) sam = 
  let (sam1,prg1') = syncAnalysis prg1 sam 
      (sam2,prg2') = syncAnalysis prg2 sam1
  in (sam2,prg1' `ProgramSeq` prg2')


-- The below case should just take place within a ForAll case.
syncAnalysis (Assign nom ix a) sam = error "should not happen" 



analyseForAll (ForAll g n) sam = (sam'',if sNeeded
                                        then Synchronize True *>* ForAll g n
                                        else ForAll g n ) 
                                 -- error$ show arrloc -- (sam',prg) 
  where                                   
    threads   = [0..(n-1)]
    gPrgs     = [(g (fromIntegral tid),tid) | tid <- threads] 
    
    arrloc''   = concatMap getSourceIndices gPrgs  
    arrloc'    = filter pred arrloc''
    arrloc     = map evalSource arrloc'
    targetMaps = map getTargIndex gPrgs 
    
    (sam',sNeeded) = conflict arrloc sam
    sam''          = addMappings targetMaps sam'
    
    
    eval (Literal a) = a 
    evalSource (n,(a,ix)) = (n,(a,eval ix))
    pred (_,(x,_)) = not ("input" `isPrefixOf` x) 
    


getSourceIndices ((Assign nom (Literal ix) a),tid) = map (\y -> (ix,y)) (collectArrayIndexPairs a)
getSourceIndices ((Assign _ ix _),tid) = error$ "getSourceIndices: " ++ show ix ++ " is not Literal"
getSourceIndices _ = error "getSourceIndices: Can only handle a very simple case so far"

getTargIndex ((Assign nom (Literal ix) a),tid) = (nom,(ix,tid)) 

-- What characterizes a conflict  
conflict :: [(Word32,(Name,Word32))] -> SAMap -> (SAMap,Bool)
conflict [] sam = (sam,False) 
conflict ((thread,(arr,ix)):xs) sam = 
  case Map.lookup arr sam of 
    Nothing  -> conflict xs sam 
    (Just m) -> case Map.lookup ix m of  
                  Nothing -> error$ "this should not be possible" ++ "\n" ++ show ix ++ "\n" ++ show m 
                  (Just j) -> 
                    if (thread `div` warpSize /= j `div` warpSize)
                    then (Map.empty,True) -- We have a conflict
                    else conflict xs sam  

                      
                  



-- What thread is computing what now
addMappings :: [(Name,(Word32,Word32))] -> SAMap -> SAMap 
addMappings [] sam = sam
addMappings ((nom,(ix,thread)):xs) sam = 
  let sam' = case Map.lookup nom sam of 
               Nothing -> Map.insert nom (Map.insert ix thread Map.empty) sam 
               (Just m) -> Map.insert nom (Map.insert ix thread m) sam 
  in addMappings xs sam'  
      

import Obsidian.GCDObsidian
import Obsidian.GCDObsidian.Kernel
import Obsidian.GCDObsidian.Store
import Obsidian.GCDObsidian.Printing
import Obsidian.GCDObsidian.Evaluate

import qualified Obsidian.GCDObsidian.CodeGen.CUDA   as CUDA
import qualified Obsidian.GCDObsidian.CodeGen.OpenCL as OpenCL
import qualified Obsidian.GCDObsidian.CodeGen.C as C 

import Prelude hiding (zipWith,splitAt)
import Data.Word
  
------------------------------------------------------------------------------
--Some tests of GCDObsidian functionality

vectorAdd :: (Array IntE, Array IntE) -> Kernel (Array IntE)   
vectorAdd (a,b) = return$ zipWith (+) a b 
  
testSync :: Array IntE -> Kernel (Array IntE) 
testSync arr = sync arr 

run1 =    
  putStrLn$ CUDA.genKernel "sync" testSync (namedArray "apa" 128)
   
run1CL =   
  putStrLn$ OpenCL.genKernel "sync" testSync (namedArray "apa" 128)
run1C =    
  putStrLn$ C.genKernel "sync" testSync (namedArray "apa" 128)

  
  
------------------------------------------------------------------------------  
--
testTwo :: Array IntE -> Kernel (Array IntE) 
testTwo arr = do 
  arr1 <- return$ twoK 1 rev arr  
  sync arr1 -- and a sync for fun

run2 = 
  putStrLn$ CUDA.genKernel "two" testTwo (namedArray "apa" 32)
   
run2CL =    
  putStrLn$ OpenCL.genKernel "two" testTwo (namedArray "apa" 32)

run2C =    
  putStrLn$ C.genKernel "two" testTwo (namedArray "apa" 32)

------------------------------------------------------------------------------
--
testComp :: Array IntE -> Kernel (Array IntE) 
testComp = (pure rev) ->- sync ->- (pure rev)

run3 = 
  putStrLn$ CUDA.genKernel "comp" testComp (namedArray "apa" 32)
   
run3CL =    
  putStrLn$ OpenCL.genKernel "comp" testComp (namedArray "apa" 32)
  
run3C =    
  putStrLn$ C.genKernel "comp" testComp (namedArray "apa" 32)


  
------------------------------------------------------------------------------    
--
testStoreIlv :: (Array IntE, Array IntE) -> Kernel (Array IntE, Array IntE) 
testStoreIlv inputs = (storeIlv ->- pure (unzipp . rev)) (zipp inputs)

run4 = 
  putStrLn$ CUDA.genKernel "storeIlv" testStoreIlv (namedArray "apa" 32,namedArray "apa" 32)
   
run4CL =    
  putStrLn$ OpenCL.genKernel "storeIlv" testStoreIlv (namedArray "apa" 32,namedArray "apa" 32)
  
run4C =    
  putStrLn$ C.genKernel "storeIlv" testStoreIlv (namedArray "apa" 32,namedArray "apa" 32)


------------------------------------------------------------------------------
--
sklansky :: Int -> Array (Data Int) -> Kernel (Array (Data Int))
sklansky 0 = pure id 
sklansky n = pure (twoK (n-1) fan) ->- store ->- sklansky (n-1)
  where fan arr = conc (a1,a2')
          where 
            (a1,a2) = splitAt middle arr  
            middle = len arr `div` 2 
            n = len a1          
            e = a1 ! (fromIntegral (n-1)) 
            a2' = (Array (\ix -> (a2 ! ix) + e) (len a2))
                    
            
run5 = 
  putStrLn$ CUDA.genKernel "sklansky" (sklansky 5) (namedArray "apa" 32)
   
run5CL =    
  putStrLn$ OpenCL.genKernel "sklansky" (sklansky 5)(namedArray "apa" 32)
  
  
run5C = 
  putStrLn$ C.genKernel "sklansky" (sklansky 5)(namedArray "apa" 32)
    

------------------------------------------------------------------------------
--
testSyncP :: (Array IntE,Array IntE) -> Kernel (Array IntE, Array IntE) 
testSyncP inputs = sync2 inputs

run6 = 
  putStrLn$ CUDA.genKernel "syncP" testSyncP (namedArray "apa" 32,namedArray "apa" 8)
   
run6CL =    
  putStrLn$ OpenCL.genKernel "syncP" testSyncP (namedArray "apa" 32, namedArray "apa" 8)
  
  
run6C = 
  putStrLn$ C.genKernel "syncP" testSyncP (namedArray "apa" 32, namedArray "apa" 8)
    

------------------------------------------------------------------------------
--
testSyncP2 :: (Array IntE,Array IntE) -> Kernel (Array IntE, Array IntE) 
testSyncP2 inputs = do 
  arr <- sync2 inputs 
  sync (fst inputs)
  return arr

run7 = 
  putStrLn$ CUDA.genKernel "syncP" testSyncP2 (namedArray "apa" 32,namedArray "apa" 8)
   
run7CL =    
  putStrLn$ OpenCL.genKernel "syncP" testSyncP2 (namedArray "apa" 32, namedArray "apa" 8)
  
  
run7C = 
  putStrLn$ C.genKernel "syncP" testSyncP2 (namedArray "apa" 32, namedArray "apa" 8)



------------------------------------------------------------------------------
--
testStoreIlvF :: (Array IntE, Array IntE) -> Kernel (Array IntE) 
testStoreIlvF inputs = storeIlvF (zipp inputs)

run8 = 
  putStrLn$ CUDA.genKernel "storeIlv" testStoreIlvF (namedArray "apa" 32,namedArray "apa" 32)
   
run8CL =    
  putStrLn$ OpenCL.genKernel "storeIlv" testStoreIlvF (namedArray "apa" 32,namedArray "apa" 32)
  
run8C =    
  putStrLn$ C.genKernel "storeIlv" testStoreIlvF (namedArray "apa" 32,namedArray "apa" 32)


------------------------------------------------------------------------------
--
testStoreCatZ :: (Array IntE, Array IntE) -> Kernel (Array IntE) 
testStoreCatZ inputs = storeCatZ (zipp inputs)

run9 = 
  putStrLn$ CUDA.genKernel "storeCat" testStoreCatZ (namedArray "apa" 32,namedArray "apa" 32)
   
run9CL =    
  putStrLn$ OpenCL.genKernel "storeCat" testStoreCatZ (namedArray "apa" 32,namedArray "apa" 32)
  
run9C =    
  putStrLn$ C.genKernel "storeCat" testStoreCatZ (namedArray "apa" 32,namedArray "apa" 32)


testEquality :: (Array IntE, Array IntE) -> Kernel (Array IntE) 
testEquality (a,b) = return$ zipWith fun a b 
  where 
    fun :: Exp Int -> Exp Int -> Exp Int
    fun x y = ifThenElse (x ==* y) 1 0 

run10 = 
  putStrLn$ CUDA.genKernel "equality" testEquality (namedArray "apa" 32,namedArray "apa" 32)
   
run10CL =    
  putStrLn$ OpenCL.genKernel "equality" testEquality (namedArray "apa" 32,namedArray "apa" 32)
  
run10C =    
  putStrLn$ C.genKernel "equality" testEquality (namedArray "apa" 32,namedArray "apa" 32)


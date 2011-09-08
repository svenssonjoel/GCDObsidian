{-# LANGUAGE TypeFamilies, 
             FlexibleInstances, 
             UndecidableInstances, 
             IncoherentInstances,
             FlexibleContexts,
             RankNTypes#-} 

module Obsidian.GCDObsidian.Evaluate where 


import Obsidian.GCDObsidian.Kernel
import Obsidian.GCDObsidian.Exp
import Obsidian.GCDObsidian.Array 
import Obsidian.GCDObsidian.Elem

import qualified Obsidian.GCDObsidian.Tuple as Tuple 
import Obsidian.GCDObsidian.Tuple (Tuple ((:.),Nil) ) 

import Data.Word
import Data.List
import qualified Data.Map as Map
------------------------------------------------------------------------------
type Env = [(Name,[Value])]

emptyEnv = []           

------------------------------------------------------------------------------
  
-- instance Scalar a => Dynamic.Typeable a 
                  
class UserData a where 
  type UserDataRepr a 
  
  fromRepr :: UserDataRepr a -> a 
  toRepr   :: a -> Env -> UserDataRepr a  
  
  
instance UserData (Exp Int) where 
  type UserDataRepr (Exp Int) = Value 
  
  fromRepr (IntVal i) = Literal i 
  toRepr  e env = evalExp e env
  
instance UserData (Exp Float) where 
  type UserDataRepr (Exp Float) = Value 
  
  fromRepr (FloatVal i) = Literal i 
  toRepr  e env = evalExp e env

instance UserData (Exp Word32) where 
  type UserDataRepr (Exp Word32) = Value 
  
  fromRepr (Word32Val i) = Literal i 
  toRepr  e env = evalExp e env

instance UserData (Exp a) => UserData (Array a) where 
  type UserDataRepr (Array a) = [UserDataRepr (Exp a)]  
  
  fromRepr as    = Array (\(Literal ix) -> (fromRepr (as !! (fromIntegral ix)))) (fromIntegral (length as) )
  toRepr arr env = [toRepr (arr ! (Literal ix)) env | ix <- [0..(len arr) - 1]]

instance (UserData a, UserData b) => UserData (a,b)  where   
  type UserDataRepr (a,b) = (UserDataRepr a,UserDataRepr b) 
  
  fromRepr (a,b)    = (fromRepr a, fromRepr b)
  toRepr  (a,b) env = (toRepr a env, toRepr b env)
    

instance Scalar a => UserData (Exp a) where 
  type UserDataRepr (Exp a) = Value 
  
  fromRepr v = Literal (fromValue v)
  toRepr i env  = evalExp i env
                 
------------------------------------------------------------------------------
-- Evaluate a kernel 
evalKernel  :: (UserData a, UserData b) 
               => (a -> Kernel b) 
               -> UserDataRepr a 
               -> UserDataRepr b
evalKernel kernel a = evalResults b finalEnv 
   where 
     finalEnv = evalCode arraySize emptyEnv code   
     ((b,(_,arraySize)),code) = runKernel (kernel (fromRepr a))



evalCode _         env Skip              = env 
evalCode arraySize env (Seq  store code) = evalCode arraySize newEnv code
  where 
    newEnv = evalStore arraySize env store  
    
    
evalStore arraySize env (Store _ ws) = evalWrites arraySize env ws      


evalWrites arraySize env ws = concatMap (evalWrite arraySize env) ws 

------------------------------------------------------------------------------
-- evalWrite. The heart of the eval function 
evalWrite arraySize env (Write targ source _) = 
  case lookup rn env  of 
    (Just arr) -> error "do something"
    Nothing -> (rn,elems) : env                      
  where
    elems   = allUpdates [ (targIx targ ix,toRepr (source ! (Literal ix)) env) 
                         | ix <- [0..staticLength source - 1 ]]
                         defList
                                 
    defList = defaultList arraySize rn (evalClosedExp (source ! (Literal 0))) -- [toRepr (source ! (Literal ix)) env | ix <- [0..staticLength source - 1 ]]
    rn = rootName (targ (variable "X"))
    
defaultList :: ArraySizes -> Name -> Value -> [Value] 
defaultList arraySize name a = 
  case (Map.lookup name arraySize) of 
    (Just n) -> replicate (fromIntegral n) a
  
    
targIx :: (Exp Word32 -> Exp a) -> Word32 -> Word32
targIx tIxf i = 
  case (tIxf (Literal i)) of 
    (Index (_,[e])) -> fromValue$ evalClosedExp e
    _               -> error "targIx: If this happens a lot, change the Write datatype"
                       
------------------------------------------------------------------------------ 
-- This one does not deserve to be a function
evalResults :: UserData b =>  b -> Env ->  UserDataRepr b
evalResults b env = toRepr b env


------------------------------------------------------------------------------
-- Updating env and lists 
    
updateEnv name val env = 
  case lookup name env of 
    (Just x) -> (name,val) : (delete (name,x) env)
    Nothing  -> (name,val) : env 
      
updateList []   _      = error "updateList"
updateList (x:xs) (0,y) = y:xs
updateList (x:xs) (i,y) = x:updateList xs (i-1,y) 


allUpdates [] a = a 
allUpdates (x:xs) a = allUpdates xs (updateList a x)


 
------------------------------------------------------------------------------
-- EvalExp 
evalClosedExp ::(UserDataRepr (Exp a) ~ Value,  UserData (Exp a)) 
                =>  Exp a 
                -> (UserDataRepr (Exp a))
evalClosedExp e = evalExp e [] 

evalExp ::(UserDataRepr (Exp a) ~ Value,  UserData (Exp a)) 
          =>  Exp a 
          -> Env 
          -> (UserDataRepr (Exp a))
evalExp (Literal a) env = toValue a 
evalExp exp@(Index (name,[e])) env = 
  case lookup name env of 
        (Just arr) -> arr !! (fromIntegral ((fromValue e') :: Word32))
        Nothing -> error "evalExp: Array not in environment"
       
  where 
    e' = evalExp e env 
evalExp a env = error "evalExp: this is not yet implemented"     


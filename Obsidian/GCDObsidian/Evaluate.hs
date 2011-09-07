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
                 
evalKernel  :: (UserData a, UserData b) => (a -> Kernel b) -> UserDataRepr a -> UserDataRepr b
evalKernel kernel a = evalResults b finalEnv 
   where 
     finalEnv = evalCode code emptyEnv  
     ((b,arraySize),code) = runKernel (kernel (fromRepr a))



evalCode Skip env = env 
evalCode (Seq  store code) env = evalCode code newEnv
  where 
    newEnv = evalStore store env 
    
evalStore (Store _ ws) env = evalWrites ws env     

evalWrites ws env = concatMap (evalWrite env) ws 

evalWrite env (Write targ source _) = 
  case lookup rn env  of 
    (Just arr) -> error "do something"
    Nothing -> (rn,elems) : env 
               
      
  where
    elems = [toRepr (source ! (Literal ix)) env | ix <- [0..staticLength source - 1 ]]
    --t  = typeOf (source ! (variable "X"))
    rn = rootName (targ (variable "X"))
       

evalResults :: UserData b =>  b -> Env ->  UserDataRepr b
evalResults b env = toRepr b env


 
------------------------------------------------------------------------------
-- EvalExp 
evalExp ::(UserDataRepr (Exp a) ~ Value,  UserData (Exp a)) =>  Exp a -> Env -> (UserDataRepr (Exp a))
evalExp (Literal a) env = toValue a 
evalExp exp@(Index (name,[e])) env = 
  case lookup name env of 
        (Just arr) -> arr !! (fromIntegral ((fromValue e') :: Word32))
        Nothing -> error "evalExp: Array not in environment"
       
  where 
    e' = evalExp e env 
evalExp a env = error "evalExp: this is not yet implemented"     


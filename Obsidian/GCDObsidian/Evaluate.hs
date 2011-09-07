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

-- import qualified Data.Typeable as Typeable
import qualified Data.Dynamic as Dynamic
import Data.Maybe

------------------------------------------------------------------------------
type Env = [(Name,[Dynamic.Dynamic])]

emptyEnv = []           


------------------------------------------------------------------------------
  
-- instance Scalar a => Dynamic.Typeable a 
                  
class UserData a where 
  type UserDataRepr a 
  
  fromRepr :: UserDataRepr a -> a 
  toRepr   :: a -> Env -> UserDataRepr a 
  
 
--instance UserData (Exp Int) where 
--  type UserDataRepr (Exp Int) = Int
  
--  fromRepr i = Literal i 
--  toRepr   e env = evalExp e env
  
instance Scalar a => UserData (Exp a) where 
  type UserDataRepr (Exp a) = a 
  fromRepr i = Literal i 
  toRepr  e env = evalExp e env
  
  
instance (Dynamic.Typeable a, Dynamic.Typeable b, 
          Elem a, Elem b, 
          UserDataRepr (Exp a) ~ a, 
          UserDataRepr (Exp b) ~ b, 
          UserData (Exp a), UserData (Exp b)) => UserData (Exp (a,b))  where 
  type UserDataRepr (Exp (a,b)) = (a,b) 
  
  fromRepr (a,b) = tup2 (fromRepr a, fromRepr b)
  toRepr e env = evalExp e env 
    
                 
instance UserData (Exp a) => UserData (Array a) where 
  type UserDataRepr (Array a) = [UserDataRepr (Exp a)] 
  
  fromRepr as = Array (\(Literal ix) -> (fromRepr (as !! (fromIntegral ix)))) (fromIntegral (length as) )
  toRepr arr env  = [toRepr (arr ! (Literal ix)) env | ix <- [0..(len arr) - 1]]
                 
                  

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

-- evalWrite :: Env -> Write 
evalWrite env (Write targ source _) = 
  case lookup rn env  of 
    (Just arr) -> error "do something"
    Nothing -> (rn,map Dynamic.toDyn elems) : env 
               
      
  where
    elems = [toRepr (source ! (Literal ix)) | ix <- [0..staticLength source - 1 ]]
    t  = typeOf (source ! (variable "X"))
    rn = rootName (targ (variable "X"))
       

evalResults :: UserData b =>  b -> Env ->  UserDataRepr b
evalResults b env = toRepr b env



------------------------------------------------------------------------------
-- EvalExp 
evalExp :: Dynamic.Typeable a => Exp a -> Env -> a
evalExp (Literal a) env = a 
evalExp exp@(Index (name,[e])) env = 
  case lookup name env of 
        (Just arr) -> --  error$ show name ++ " " ++ (show arr) ++ "  " ++ show (fromIntegral e')
             case  (Dynamic.fromDynamic$ arr !! (fromIntegral e')) of 
               (Just a) -> a
               Nothing  -> error "Nothing" 
                        
        Nothing -> error "evalExp: Array not in environment"
       
  where 
    e' = evalExp e env 
evalExp a env = error "evalExp: this is not yet implemented"     


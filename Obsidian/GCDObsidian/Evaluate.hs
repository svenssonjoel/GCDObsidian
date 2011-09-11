
{-# LANGUAGE TypeFamilies, 
             FlexibleInstances,
             FlexibleContexts, 
             UndecidableInstances #-} 

module Obsidian.GCDObsidian.Evaluate where


import Data.List
import Data.Word


import Obsidian.GCDObsidian.Array
import Obsidian.GCDObsidian.Exp 

------------------------------------------------------------------------------

data EValue 
     = EValInt    Int 
     | EValFloat  Float
     | EValBool   Bool 
     | EValWord32 Word32


------------------------------------------------------------------------------
type family EvalIO a 
type instance EvalIO (Exp Int)    = EValue 
type instance EvalIO (Exp Float)  = EValue 
type instance EvalIO (Exp Bool)   = EValue 
type instance EvalIO (Exp Word32) = EValue

type instance EvalIO (Array a)    = [EvalIO (Exp a)]
type instance EvalIO (a,b)        = (EvalIO a, EvalIO b)
type instance EvalIO (Exp (a,b))  = (EvalIO (Exp a), EvalIO (Exp b))


------------------------------------------------------------------------------
type Env = [(Name,[Value])] -- for now

class Eval a where 
  evaluate :: Env -> a -> (EvalIO a) 
  deEvaluate :: EvalIO a -> a
  
  
instance Eval (Exp Int) where 
  evaluate env exp = evalExp env exp  
  deEvaluate (EValInt i) = Literal i
  
instance Eval (Exp Word32) where 
  evaluate env exp = evalExp env exp  
  deEvaluate (EValWord32 i) = Literal i



instance Eval (Exp a) => Eval (Array a) where
  evaluate env arr = [evaluate env (arr ! (Literal i)) | i <- [0..len arr-1]]
  deEvaluate ls = Array (\ix -> deEvaluate (ls `wordIx` (evaluate [] ix))) (fromIntegral (length ls))

wordIx ls (EValWord32 w) = ls !! (fromIntegral w)

evalExp = undefined
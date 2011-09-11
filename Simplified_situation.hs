
{-# LANGUAGE TypeFamilies, 
             FlexibleInstances,  
             FlexibleContexts,
             UndecidableInstances,
             GADTs #-} 

module Simplified_situation where 

import Data.List


data Expr a where
  Literal :: a -> Expr a 
  Tuple :: (Expr a) -> (Expr b) -> Expr (a,b) 


data Array a = Array (Int -> (Expr a)) Int 
len (Array _ n) = n
(!) (Array ixf _) ix = ixf ix



program :: Array Int -> Array Int 
program arr = Array (\ix -> (arr ! (len arr - 1 - ix))) (len arr)

program2 :: (Array Int,Array Int) -> Array Int 
program2 (arr1,arr2) = program arr1

zipper :: (Array Int, Array Int) -> Array (Int,Int) 
zipper (a1,a2) = Array (\ix -> Tuple (a1 ! ix) (a2 ! ix)) (len a1)


type family ExecIO a 
type instance ExecIO (Expr Int)   = Int 
type instance ExecIO (Array a)    = [ExecIO (Expr a)]
type instance ExecIO (a,b)        = (ExecIO a, ExecIO b) 
type instance ExecIO (Expr (a,b)) = (ExecIO (Expr a),ExecIO (Expr b))

class Exec a where 
  toExec :: ExecIO a -> a 
  fromExec :: a -> ExecIO a
  
instance Exec (Expr Int) where   
  toExec i = Literal i 
  fromExec (Literal i) = i 
  
instance Exec (Expr a) => Exec (Array a) where 
  toExec ls = Array (\ix -> toExec (ls !! ix)) (length ls) 
  fromExec arr = [fromExec (arr ! i) | i <- [0.. len arr-1]]

instance (Exec a, Exec b) => Exec (a,b) where 
  toExec (a,b) = (toExec a, toExec b) 
  fromExec (a,b) = (fromExec a, fromExec b) 

instance (Exec (Expr a), Exec (Expr b)) => Exec (Expr (a,b)) where
  toExec (i1,i2) = Tuple (toExec i1)  (toExec i2)
  fromExec (Tuple e1 e2) = (fromExec e1,fromExec e2)
         


execute :: (Exec a, Exec b)  => (a -> b) -> (ExecIO a) -> (ExecIO b) 
execute prg input = fromExec$ prg (toExec input)
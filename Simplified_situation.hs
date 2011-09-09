
{-# LANGUAGE TypeFamilies, 
             FlexibleInstances #-} 

module Test1 where 

import Data.List




data Array a = Array (Int -> a) Int 
len (Array _ n) = n
(!) (Array ixf _) ix = ixf ix



program :: Array Int -> Array Int 
program arr = Array (\ix -> (arr ! (len arr - 1 - ix))) (len arr)

program2 :: (Array Int,Array Int) -> Array Int 
program2 (arr1,arr2) = program arr1

zipper :: (Array Int, Array Int) -> Array (Int,Int) 
zipper (a1,a2) = Array (\ix -> (a1 ! ix, a2 ! ix)) (len a1)


type family ExecIO a 
type instance ExecIO Int         = Int 
type instance ExecIO (Array a)   = [ExecIO a]
type instance ExecIO (a,b) = (ExecIO a, ExecIO b) 

class Exec a where 
  toExec :: ExecIO a -> a 
  fromExec :: a -> ExecIO a
  
instance Exec Int where   
  toExec i = i 
  fromExec i = i 
  
instance Exec a => Exec (Array a) where 
  toExec ls = Array (\ix -> toExec (ls !! ix)) (length ls) 
  fromExec arr = [fromExec (arr ! i) | i <- [0.. len arr-1]]

instance (Exec a, Exec b) => Exec (a,b) where 
  toExec (a,b) = (toExec a, toExec b) 
  fromExec (a,b) = (fromExec a, fromExec b) 



execute :: (Exec a, Exec b)  => (a -> b) -> (ExecIO a) -> (ExecIO b) 
execute prg input = fromExec$ prg (toExec input)
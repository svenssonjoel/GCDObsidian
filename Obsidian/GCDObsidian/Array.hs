{-# LANGUAGE MultiParamTypeClasses, 
             FlexibleInstances, 
             FlexibleContexts, 
             UndecidableInstances, 
             RankNTypes, 
             GADTs #-} 

module Obsidian.GCDObsidian.Array ((!)
                                  , namedArray
                                  , indexArray
                                  , len 
                                  , Array(..)  
                                  , Program(..)  
                                  , pushApp
                                  , targetArray
                                  , toArrayP
                                  , toArrayP'
                                  , concP
                                  , revP
                                  , ArrayP(..)
  
                                  )where 

import Obsidian.GCDObsidian.Elem
import Obsidian.GCDObsidian.Tuple
import Obsidian.GCDObsidian.Exp 

import Data.List
import Data.Word

------------------------------------------------------------------------------
-- Arrays!


type Dynamic = Bool 

data Array a = Array (Exp Word32 -> a) Word32 


-- PUSHY ARRAYS 
data ArrayP a = ArrayP ((Exp Word32 -> (a -> Program)) -> Program) Word32

pushApp (ArrayP func n) a = func a 

------------------------------------------------------------------------------

revP :: ArrayP a -> ArrayP a 
revP (ArrayP h n) = ArrayP (revHelp (\ix -> (fromIntegral (n-1)) - ix) h) n 
  where 
    revHelp f p = \func -> p (\i -> func (f i))
    
    
concP :: ArrayP a -> ArrayP a -> ArrayP a     
concP (ArrayP f n1) (ArrayP g n2) = 
  ArrayP (\func -> ProgramSeq ( f func )
                              ( g (\i -> func (fromIntegral n1 + i))))
                       (n1+n2)

------------------------------------------------------------------------------ 
-- Scalars for now. Learn how to loosen that demand
data Program 
  = forall a. Scalar a => Assign Name (Data Word32) (Data a)
  | ForAll (Data Word32 -> Program) Word32
  | Allocate Name Word32 Type Program 
  | ProgramSeq Program Program  
  
    
printProgram :: Program -> String 
printProgram (Assign n t e) = n ++ "[" ++ show t ++ "]" ++ " = " ++ show e ++ ";\n"  
printProgram (ForAll f n)   = "forall i 0.." ++ show n ++ " {\n" ++ printProgram (f (variable "tid")) ++ "\n}" 
printProgram (Allocate name n t p) = "name = malloc(" ++ show n ++ ")\n" ++ printProgram p
printProgram (ProgramSeq p1 p2) = printProgram p1 ++ printProgram p2
    
instance Show Program where 
  show = printProgram 
         
class Pushable a e where 
  toArrayP :: a e -> ArrayP e 
  toArrayP' :: Word32 -> a e -> ArrayP e  


instance Pushable ArrayP e where 
  toArrayP = id 
  toArrayP' = undefined
  
instance Pushable Array e where   
  toArrayP (Array ixf n) = ArrayP (\func -> ForAll (\i -> func i (ixf i)) n) n 
  toArrayP' m (Array ixf n) = 
    ArrayP (\func -> ForAll (\i -> foldr1 ProgramSeq [func (i+(fromIntegral j))  
                                                      (ixf (i+(fromIntegral j)))
                                                     | j<-  [0..m-1]
                                                     ]) (n `div` m)) n

targetArray :: Scalar a => Name -> Exp Word32 -> (Exp a -> Program)
targetArray n i = \a -> Assign n i a 


------------------------------------------------------------------------------
--

namedArray name n = Array (\ix -> index name ix) n 
indexArray n      = Array (\ix -> ix) n 

class Indexible a e where 
  access :: a e -> Exp Word32 -> e 
  
instance Indexible Array a where
  access (Array ixf _) ix = ixf ix


len :: Array a -> Word32
len (Array _ n) = n 

(!) :: Indexible a e => a e -> Exp Word32 -> e 
(!) = access


------------------------------------------------------------------------------
-- Show 

instance Show  a => Show (Array a) where
  show arr | len arr <= 10 =  "[" ++ 
                              (concat . intersperse ",") 
                              [show (arr ! (fromIntegral i)) | i <- [0..len arr-1]] ++ 
                              "]"
           | otherwise     =  "[" ++ 
                              (concat . intersperse ",") 
                              [show (arr ! (fromIntegral i)) | i <- [0..3]] ++ 
                              "...]"

         
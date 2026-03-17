module Common where
import Data.Map     as D
import Control.Monad (liftM , ap)
import Data.Maybe
import Prelude


newtype ST a = ST {runST :: (TEnv -> (a,TEnv))}

returnT :: a -> ST a
returnT x = ST (\e-> (x,e))
instance Monad ST where 
  return = returnT
  m >>= f = ST (\e -> let (v,e') = runST m e in  runST (f v) e') 

instance Functor ST where
  fmap = liftM

instance Applicative ST where
  pure  = returnT
  (<*>) = ap

type TEnv = Map String TypeEnv

newtype SE a = SE {runSE :: EEnv -> (a, EEnv)}

returnE :: a -> SE a
returnE x = SE (\e -> (x,e))

instance Monad SE where
return = returnE
m >>= f = SE (\e -> let (v,s) = runSE m e in runSE (f v) s)

instance Functor SE where
  fmap = liftM

instance Applicative SE where
  pure  = returnE
  (<*>) = ap

type EEnv = Map String ExpEnv

data Defs = DefT String [String] Type
          | Def String Term
          deriving (Show)

data Type = EmptyT
          | VarT String
          | UniT String Type 
          | FunT Type Type 
          deriving (Show, Eq)  

data Term    = EVar String
             | EAbs String Type Term 
             | EApp Term Term
             | ETAbs String Term
             | ETApp Term Type 
             deriving (Show, Eq)

data LocalTerm  = Bound Int
                | FreeV String 
                | App LocalTerm LocalTerm
                | Lam Type LocalTerm
                | LamT String LocalTerm
                | AppT LocalTerm Type 
            deriving (Show, Eq)

data Value = VLam Type LocalTerm 
           | VType Type LocalTerm
           deriving (Show, Eq)


type ExpEnv  = (Value,Type)
type TypeEnv = ([String],Type)



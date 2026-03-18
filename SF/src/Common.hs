module Common where
import Data.Map     as D
import Control.Monad (liftM , ap)
import Data.Maybe
import Prelude

type Env = D.Map String (Either ExpEnv TypeEnv)

newtype State a = State {runState :: Env -> Either Error (a,Env)}

instance Monad State where
  return x = State(\e -> Right (x,e))
  m >>= f  = State(\e -> case runState m e of
                         Left err    -> Left err
                         Right (v,s) -> runState (f v) s
                  ) 

instance Functor State where
  fmap = liftM

instance Applicative State where
  pure  = return
  (<*>) = ap 

data Error = ExpVarNotFound | TypeVarNotFound | InvalidApp | BadDef deriving (Show,Eq)


data Defs = DefT Type Type
          | Def String Term
          deriving (Show)

data Type = VarT String
          | UniT Type Type 
          | FunT Type Type 
          deriving (Show, Eq)  

data Term    = EVar String
             | EAbs String Type Term 
             | EApp Term Term
             | ETAbs Type Term
             | ETApp Term [Type] 
             deriving (Show, Eq)

data LocalTerm  = Bound Int
                | FreeV String 
                | App LocalTerm LocalTerm
                | Lam Type LocalTerm
                | LamT Type LocalTerm
                | AppT LocalTerm Type 
            deriving (Show, Eq)

data Value = VLam Type LocalTerm 
           | VType Type LocalTerm
           deriving (Show, Eq)

type ExpVar = String
type TypeVar = String

type ExpEnv  = (Value,Type)
type TypeEnv = ([String],Type)



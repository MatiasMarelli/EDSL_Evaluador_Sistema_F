module Common where
import Data.Map     as M
import Control.Monad (liftM , ap)
import Data.Maybe
import Prelude
import Control.Monad.IO.Class

type Env = M.Map String (Either ExpEnv TypeEnv)

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


------------------------------------------------------------------------------------------


lookForExp :: String ->State Value
lookForExp n = State (\e -> case M.lookup n e of
                            Nothing             -> Left ExpVarNotFound
                            Just (Right _)      -> Left BadExpDef
                            Just (Left (v,_))   -> Right (v , e)
                     )

updateExpEnv :: String -> Value -> Type -> State ()
updateExpEnv n v t = State(\e -> Right ((),M.insert n (Left (v,t)) e))

updateTypeEnv :: String -> Type -> State ()
updateTypeEnv n t  = State (\e -> Right ((), M.insert n (Right t) e)) 


lookForType :: String -> State Type
lookForType n = State (\e -> case M.lookup n e of
                             Nothing        -> Left TypeVarNotFound
                             Just (Left _)  -> Left BadTypeDef
                             Just (Right t) -> Right (t,e))

getState :: State Env
getState = State(\e -> Right(e,e) )

throwApp :: State Value
throwApp = State(\_ -> Left InvalidApp)

throwTypeDef :: State Type
throwTypeDef = State(\_ -> Left BadTypeDef)
throwTypeApp :: State LocalType
throwTypeApp = State(\_ -> Left InvalidTypeApp)

throwExpDef :: State a
throwExpDef = State(\_ -> Left BadExpDef)

throwPar :: String -> State a
throwPar s = State(\_ -> Left (Par s))
------------------------------------------------------------------------------------------



data Error = ExpVarNotFound | TypeVarNotFound| Par String | InvalidApp| InvalidTypeApp | BadExpDef | BadTypeDef deriving (Show,Eq)


data Defs = DefT Type [Type] Type
          | TypeExp String Type
          | Def String Term
          deriving (Show)

data Type = VarT String
          | UniT String Type 
          | FunT Type Type 
          deriving (Show, Eq)  

data LocalType = BoundT Int
               | FreeT String
               | Uni LocalType
               | Fun LocalType LocalType
               deriving (Show,Eq)

data Term    = EVar String
             | EAbs String Type Term 
             | EApp Term Term
             | ETAbs Type Term
             | ETApp Term [Type] 
             deriving (Show, Eq)

data LocalTerm  = Bound Int
                | FreeV String 
                | App LocalTerm LocalTerm
                | Lam LocalType LocalTerm
                | LamT LocalTerm
                | AppT LocalTerm [LocalType] 
            deriving (Show, Eq)

data Value = VLam LocalType LocalTerm 
           | VType LocalTerm
           deriving (Show, Eq)

type ExpVar  = String
type TypeVar = String

type ExpEnv  = (Value,Type)
type TypeEnv = Type



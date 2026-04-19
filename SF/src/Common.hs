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
                            Just (Right _)      -> Left (BadExpDef ("Variable " ++ n ++ " was not an expresion")) 
                            Just (Left (v,_))   -> Right (v , e)
                     )

updateExpEnv :: String -> Value -> Type -> State ()
updateExpEnv n v t = State(\e -> Right ((),M.insert n (Left (v,t)) e))

updateTypeEnv :: String -> Type -> State ()
updateTypeEnv n t  = State (\e -> Right ((), M.insert n (Right t) e)) 


lookForType :: String -> State Type
lookForType n = State (\e -> case M.lookup n e of
                             Nothing        -> Left (TypeVarNotFound n)
                             Just (Left _)  -> Left BadTypeDef
                             Just (Right t) -> Right (t,e))

getState :: State Env
getState = State(\e -> Right(e,e) )

throwApp :: String -> State Value
throwApp s = State(\_ -> Left (InvalidApp s))

throwTypeDef :: State Type
throwTypeDef = State(\_ -> Left BadTypeDef)

throwTypeApp ::String -> State LocalType
throwTypeApp s = State(\_ -> Left (InvalidTypeApp s))

throwExpDef :: String -> State a
throwExpDef s = State(\_ -> Left (BadExpDef s))

throwPar :: String -> State a
throwPar s = State(\_ -> Left (Par s))
------------------------------------------------------------------------------------------



data Error = ExpVarNotFound | TypeVarNotFound String | Par String | InvalidApp String| InvalidTypeApp String | BadExpDef String | BadTypeDef deriving (Show,Eq)


data Defs = DefT Type [Type] Type
          | TypeExp String Type
          | Def String Term
          deriving (Show)

data Type = VarT String [Type]
          | UniT String Type 
          | FunT Type Type 
          deriving (Show, Eq)  

data LocalType = BoundT Int
               | FreeT String [LocalType]
               | Uni LocalType
               | Fun LocalType LocalType
               deriving (Show)
instance Eq LocalType where
  BoundT i   == BoundT j    = if i == j then True else False
  FreeT n xs == FreeT n' ys = if n == n' then xs == ys else False
  Uni t      == Uni t'      = t == t'
  Fun t1 t2  == Fun t1' t2' = t1==t1' && t2==t2'
  _          == _           = False

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



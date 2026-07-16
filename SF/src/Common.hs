module Common where
import Data.Map     as M
import Control.Monad (liftM , ap)
import Data.Maybe
import Prelude
import Control.Monad.IO.Class


--------------------------------------------------------------------------------------------
--DEFINICION DE LA MONADA

type Env = M.Map String (Either ExpEnv TypeEnv)
type Error = String

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

--FUNCIONES DE LA MONADA

err :: String -> Either Error a
err = Left

--Busca en el State una variable de Expresion 

lookForExp :: String ->State Value
lookForExp n = State (\e -> case M.lookup n e of
                            Nothing             -> err  (typeVarNotFound n)
                            Just (Right _)      -> err  $"Variable " ++ n ++ " was not an expression"
                            Just (Left (v,_))   -> Right (v , e)
                     )

--Actualiza la lista de variables de termino

updateExpEnv :: String -> Value -> Type -> State ()
updateExpEnv n v t = State(\e -> Right ((),M.insert n (Left (v,t)) e))

--Actualiza la lista de variables de tipos

updateTypeEnv :: String -> Type -> State ()
updateTypeEnv n t  = State (\e -> Right ((), M.insert n (Right t) e)) 

--Busca en el State una Variable de tipo

lookForType :: String -> State Type
lookForType n = State (\e -> case M.lookup n e of
                             Nothing        -> err (typeVarNotFound n)
                             Just (Left _)  -> err $ "Bad Type "++ n ++" definition"
                             Just (Right t) -> Right (t,e))

--Retorna el State interno de la monada
getState :: State Env
getState = State(\e -> Right(e,e) )

--Setea un State dentro de la monada
setState :: Env -> State ()
setState e = State(\_ -> Right ((),e))

--Funciones de Errores 

throwApp :: String -> State a
throwApp s = State(\_ -> err $ "Unknown expression: "++s)

typeVarNotFound :: String -> String
typeVarNotFound n = "Variable " ++ n ++ " not found"

throwTypeApp ::String -> State a
throwTypeApp s = State(\_ -> err $ "Invalid Type application, wasnt expecting :"++s)

throwExpDef :: String -> State a
throwExpDef s = State(\_ -> err $ "Error in the definition of: "++ s)

throwPar :: String -> State a
throwPar s = State(\_ -> err $ "Parse error : "++ s)

throwCompile :: String -> State a 
throwCompile s = State(\_ -> err $ "Compiling error, "++ s)
------------------------------------------------------------------------------------------

--ASTs



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

instance Eq LocalType where
  BoundT i   == BoundT j    = if i == j then True else False
  FreeT n xs == FreeT n' ys = if n == n' then xs == ys else False
  Uni t      == Uni t'      = t == t'
  Fun t1 t2  == Fun t1' t2' = t1==t1' && t2==t2'
  _          == _           = False


module TypeCheck where
import Common
import Prelude hiding (return,(>>=))
import Data.Map as M
import Eval (evalType, instanceTerm,convertionType,eval)

data TypedLocalTerm = TBound Int
                    | TFreeV String 
                    | TApp LocalTerm LocalTerm
                    | TLam Type LocalTerm
                    | TLamT LocalTerm
                    | TAppT LocalTerm [Type] 
                deriving (Show, Eq)

type Err = String

type TypeChecker a = Either Err a

return :: LocalType -> TypeChecker LocalType
return  = Right

(>>=) :: TypeChecker LocalType -> (LocalType -> TypeChecker LocalType) -> TypeChecker LocalType
m >>= f = case m of 
            Right a -> f a 
            Left _ -> m



err :: String -> TypeChecker LocalType
err = Left


functionError :: LocalType -> LocalType -> LocalType -> String
functionError t1 t2 t' = ("Expected a (" ++ show (t2) ++ " -> " ++ show(t')++ ")     But got a "++ show(t1))

appType :: LocalTerm -> Env -> [LocalType] -> Either String LocalTerm
appType l e t = case (runState (evalType t) e ) of
                  Left e'      -> Left (show e')
                  Right (t',e') -> case runState(eval l) e' of
                                     Left e'' -> Left (show e'')
                                     Right (VType v,_) -> (case (runState (instanceTerm 0 v t') e) of
                                                             Left e'      -> Left (show e')
                                                             Right (x,_) -> Right x 
                                                          )

checkType :: Env -> LocalTerm -> TypeChecker LocalType 
checkType = check []

check :: [LocalType] -> Env -> LocalTerm -> TypeChecker LocalType
check c _ (Bound i)   = return (c !! i)
check _ e (FreeV n)   = case M.lookup n e of
                          Just(Left(_,t)) -> return (convertionType t)
                          _               -> err ("NotFound "++ show(n) ++ "\n")
check c e (App l1 l2) = check c e l1 >>= (\t1 -> check c e l2 >>= \t2 ->
                                            case t1 of 
                                              (Fun t t') -> if t == t2 then return t' else err (functionError t1 t2 t')
                                              _          -> err ("Expected a Function but receive "++ show(t1))
                                              )
check c e (Lam t l)   = check (t:c) e l >>= (\t' -> return (Fun t t')) 
check c e (LamT l)    = check c e l >>= (\t  -> return (Uni t))
check c e (AppT l ts) = case appType l e ts of
                          Left e'  -> err e'
                          Right v -> check c e v 

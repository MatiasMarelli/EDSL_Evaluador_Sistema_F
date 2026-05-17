module TypeCheck where
import Common hiding (err)
import Prelude hiding (return,(>>=))
import Data.Map as M
import Data.List as L
import Eval (evalType, convertionType,typeSub)

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

updateContext :: Int ->LocalType -> LocalType
updateContext i (BoundT j)   = (BoundT (j+i))
updateContext i (FreeT n xs) = FreeT n (L.map (updateContext i) xs)
updateContext i (Uni t)      = Uni (updateContext i t)
updateContext i (Fun t1 t2)  = Fun (updateContext i t1) (updateContext i t2)


functionError :: LocalType -> LocalType -> String
functionError t1 t2 = "Expected a (" ++ show (t1)  ++") But got a "++ show(t2)

appType :: Env -> [LocalType] -> Either String LocalType
appType e t = case (runState (evalType t) e ) of
                  Left e'      -> Left (show e')
                  Right (t',_) -> Right t'

checkType :: Env -> LocalTerm -> TypeChecker LocalType 
checkType = check (-1) []

check :: Int -> [(LocalType,Int)] -> Env -> LocalTerm  -> TypeChecker LocalType
check j c _ (Bound i)      = let (t,n) = (c !! i) in return (updateContext (j-n) t)
check _ _ e (FreeV n)      = case M.lookup n e of
                              Just(Left(_,t)) -> return (convertionType t)
                              _               -> err ("NotFound "++ show(n))
check i c e ñ@(App l1 l2)  = check i c e l1 >>= (\t1 -> check i c e l2 >>= \t2 -> 
                                                  case t1 of 
                                                    (Fun t t') -> if t == t2 then return t' 
                                                                             else case appType e [t] of
                                                                                    Left e'   -> err e'
                                                                                    Right t'' -> if t'' == t2 then return t'
                                                                                                              else case appType e [t2] of
                                                                                                                    Left e'   -> err e'
                                                                                                                    Right t''' -> if t'' == t''' then return t' else err $ functionError t'' t''' ++ " in " ++ show(ñ)
                                                    _          -> err $ "Expected a Function but receive "++ show(t1) ++ " .In " ++ show ñ 
                                                  )
check i c e (Lam t l)     = check i ((t,i):c) e l >>= (\t' -> return (Fun t t')) 
check i c e (LamT l)      = check (i+1) c e l  >>= (\t  -> return (Uni t))
check i c e (AppT l ts)   = case appType e ts of
                              Left e'  -> err e'
                              Right t' -> check i c e l >>= (\t'' -> case t'' of
                                                                       (Uni t''')  -> return (typeSub 0 t' t''')
                                                                       (FreeT _ _) -> case appType e [t''] of
                                                                                        Left e'' -> err e''
                                                                                        Right (Uni tt) -> return (typeSub 0 t' tt)
                                                                                        _ -> err ("No instance allowed in "++ show(t''))
                                                                       _           -> err ("No instance allowed in "++ show(t'')))                 
                                        

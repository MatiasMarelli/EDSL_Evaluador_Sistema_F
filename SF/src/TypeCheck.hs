module TypeCheck  where
import Common hiding (err)
import Prelude hiding (return,(>>=))
import Data.Map as M  (lookup)
import Data.List as L (map,elem)
import Eval (evalType, convertionType,typeSub)

data CheckerType = CDef String [CheckerType]
                 | CBound String 
                 | CUni String CheckerType 
                 | CFun CheckerType CheckerType
                 deriving(Show,Eq)
data CheckerTerm = CVar String
                 | CAbs String CheckerType CheckerTerm 
                 | CApp CheckerTerm CheckerTerm
                 | CTAbs CheckerType CheckerTerm
                 | CTApp CheckerTerm [CheckerType]
                 deriving (Show,Eq)


listType :: Type
listType = UniT "X" (UniT "R" (FunT (FunT (VarT "X" []) (FunT (VarT "R" []) (VarT "R" []))) (FunT (VarT "R" []) (VarT "R" []))))

type Err = String

type TypeChecker a = Either Err a

return :: a -> TypeChecker a
return  = Right

(>>=) :: TypeChecker a -> (a -> TypeChecker b) -> TypeChecker b
m >>= f = case m of 
            Right a -> f a 
            Left e  -> err e



err :: String -> TypeChecker a
err = Left

convertionT :: Term -> CheckerTerm
convertionT = convertion' []

convertion' :: [String] -> Term -> CheckerTerm
convertion' _ (EVar n)     = CVar n
convertion' c (EAbs n t l) = CAbs n (type2CheckType' c t) (convertion' c l)
convertion' c (EApp l1 l2) = CApp (convertion' c l1) (convertion' c l2)
convertion' c (ETAbs t l)  = case t of
                              (VarT n []) -> CTAbs (CBound n) (convertion' (n:c) l)
                              _           -> undefined   
convertion' c (ETApp l xs) = CTApp (convertion' c l) (L.map (type2CheckType' c) xs)

type2CheckType :: Type -> CheckerType
type2CheckType = type2CheckType' [] 

type2CheckType' :: [String] -> Type -> CheckerType
type2CheckType' c (UniT v t)   = CUni v (type2CheckType' (v:c) t)
type2CheckType' c (VarT n ls)  | L.elem n c  = CBound n
                               | otherwise = CDef n (L.map (type2CheckType' c) ls)
type2CheckType' c (FunT t1 t2) = CFun (f t1) (f t2)
                                 where f = type2CheckType' c 

lookContext :: String -> [(String,CheckerType)] -> Maybe CheckerType
lookContext n []     = Nothing
lookContext n (x:xs) | n == (fst x) = Just $ snd x  
                     | otherwise    = lookContext n xs

instanceType :: CheckerType -> [CheckerType] -> TypeChecker CheckerType
instanceType t []               = return t
instanceType (CUni n t') (t:ts) = subType'' n t t' >>= \x -> instanceType x ts  
instanceType _ _                = err $ "Expecting a forall for instance"

subType :: CheckerTerm -> [CheckerType] -> TypeChecker CheckerTerm
subType l []                        = return l
subType (CTAbs (CBound n) l) (t:ts) = subType' n t l >>= \t' -> subType t' ts
subType l _                         = err $ "Bad Instantiation, more Types than Universal in: "++ show(l)

map' :: String -> CheckerType -> [CheckerType] -> TypeChecker [CheckerType]
map' n t []     = return []
map' n t (x:xs) = subType'' n t x >>= (\t' -> map' n t xs >>= \ts -> return (t': ts))

subType' :: String -> CheckerType -> CheckerTerm -> TypeChecker CheckerTerm
subType' _ _ (CVar n)              = return $ CVar n
subType' n t (CAbs n' t' l)        = subType'' n t t' >>= (\x ->
                                     subType' n t l   >>= (\y -> return $ CAbs n' x y))  
subType' n t (CApp l1 l2)          = subType' n t l1  >>= (\x ->
                                     subType' n t l2  >>= (\y -> return $ CApp x y ))
subType' n t (CTAbs (CBound n') l) | n == n'   = return $ CTAbs (CBound n') l
                                   | otherwise = subType' n t l >>= \x -> return $ CTAbs (CBound n') x
subType' n t (CTApp l xs)          = map' n t xs >>= (\ts -> subType' n t l >>= \l' -> return $ CTApp l' ts)



subType'' :: String -> CheckerType -> CheckerType -> TypeChecker CheckerType
subType'' n t (CDef n' xs)             = map' n t xs >>= \ts -> return $ CDef n' ts
subType'' n t (CBound n')  | n' == n   = return t
                           | otherwise = return (CBound n')
subType'' n t (CUni n' t') | n' == n   = return $ CUni n' t'
                           | otherwise = subType'' n t t' >>= \t'' -> return $ CUni n' t'' 
subType'' n t (CFun t1 t2 )            = subType'' n t t1 >>= \t1' -> subType'' n t t2 >>= \t2' -> return $ CFun t1' t2'



infer :: Env -> CheckerTerm -> TypeChecker CheckerType
infer e l = infer' e [] l 

infer':: Env -> [(String,CheckerType)] -> CheckerTerm -> TypeChecker CheckerType
infer' e c (CVar n)     = case lookContext n c of 
                          Just t  -> return t
                          Nothing -> case M.lookup n e of
                                      Just (Left (_,t)) -> return (type2CheckType t)
                                      Nothing           -> err $ "Variable: " ++ show(n) ++ " not found"
infer' e c (CAbs n t l) = infer' e ((n,t):c) l >>= \t' -> return $ CFun t t' 
infer' e c a@(CApp l1 l2) = infer' e c l1 >>= (\t1 ->
                          infer' e c l2 >>= (\t2 ->
                           case t1 of 
                             CFun t1' t1'' -> if t1' == t2 
                                              then return t1''
                                              else err $ "Expected a " ++ show(t1') ++ " but receive a "++show(t2) ++ " in " ++ show(l1) ++ "            " ++ show(l2)
                             _             -> err $ "Expected a Function but receive: " ++ show(t1) ++ " in " ++ show(l1)))
infer' e c (CTAbs t l)  = case t of 
                          (CBound n)  -> infer' e c l >>= \t' -> return $ CUni n t'
                          _           -> err $ "Not expected a: "++ show(t) ++ " next to TypeAbstraction"
infer' e c (CTApp l ts) =  infer' e c l >>= \t' -> case t' of 
                                                     CDef n xs -> case M.lookup n e of 
                                                                    Just(Right t) -> instanceType (type2CheckType t) (xs ++ ts)
                                                                    _             -> err $"Variable "++ (show n ) ++ " not defined"
                                                     CUni n t  -> instanceType t' ts
                                                     _         -> err $ "Bad instantiation didnt expect "++ show(t')                     




isConstructor :: Env -> CheckerType -> TypeChecker CheckerType
isConstructor e (CFun t' t) = isConstructor e t >>= \t'' -> return $ CFun t' t'' 
isConstructor e (CDef n xs) = case M.lookup n e of 
                                Just(Right t) -> instanceType (type2CheckType t) xs
                                _             -> err $"Variable "++ (show n ) ++ " not defined"
isConstructor e (CUni n t)  = isConstructor e t >>= \t' -> return (CUni n t')
isConstructor _ t           = return t

{- updateContext :: Int ->LocalType -> LocalType
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
                                        
 -}
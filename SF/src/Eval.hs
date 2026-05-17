module Eval where
import Common 
import Data.Map    as M
import Data.List   as L


convertion :: Term -> LocalTerm

convertion = convertionExp' [] []
----------------------------------------------------------
convertionExp' :: [String] -> [String] -> Term -> LocalTerm

convertionExp' xs ts (EAbs n t l) = Lam (convertionType' ts t) (convertionExp' (n:xs) ts l)

convertionExp' xs ts (EApp t1 t2) = let (v1,v2) = (fun t1,fun t2)
                                    in App v1 v2
                                    where fun = (convertionExp' xs ts)
-- holiiiii
convertionExp' xs ts (ETAbs (VarT n []) l)  = LamT (convertionExp' xs (n:ts) l)

convertionExp' xs ts (ETApp l t) = AppT (convertionExp' xs ts l) (L.map (convertionType' ts) t)

convertionExp' xs ts (EVar n)     = case look xs n of
                                   Nothing -> FreeV n
                                   Just i -> Bound i

-- hola te amoooo :D (el colo)
----------------------------------------------------------

look :: [String] -> String -> Maybe Int
look = findBound 0 

findBound :: Int -> [String] -> String -> Maybe Int
findBound _ [] _ = Nothing
findBound i (x:xs) n = if x == n then Just i
                       else findBound (i+1) xs n

-----------------------------------------------------------

convertionType :: Type -> LocalType
convertionType = convertionType' []

convertionType' :: [String] -> Type -> LocalType
convertionType' xs (UniT n t)   = Uni (convertionType' (n:xs) t)
convertionType' xs (FunT t1 t2) = Fun (convertionType' xs t1) (convertionType' xs t2)
convertionType' xs (VarT n ts)     = case look xs n of
                                       Nothing -> FreeT n (L.map (convertionType' xs) ts)
                                       Just i  -> BoundT i


-----------------------------------------------------------------------------------------------

expSub :: Int -> LocalTerm -> LocalTerm -> LocalTerm 
expSub i t (Bound j)  | i == j    = t
                      | otherwise = Bound j
expSub _ _ (FreeV n)              = FreeV n
expSub i t (App t1 t2)            = App (expSub i t t1) (expSub i t t2)
expSub i t (Lam t' u)             = Lam t' (expSub (i+1) t u)
expSub i t (LamT u)               = LamT (expSub i t u)
expSub i t (AppT u t')            = AppT (expSub i t u) t'


quote :: Value -> LocalTerm
quote (VLam t l)  = Lam  t l
quote (VType l)   = LamT l  

typeSub :: Int -> LocalType -> LocalType -> LocalType
typeSub i t (BoundT j) | i == j    = t
                       | otherwise = BoundT j
typeSub i t (FreeT n ts)           = FreeT n (L.map (typeSub i t) ts) 
typeSub i t (Uni t')               = case t of 
                                       BoundT j -> Uni (typeSub (i+1) (BoundT (j+1)) t')
                                       _        -> Uni (typeSub (i+1) t t')
typeSub i t (Fun t1 t2)            = Fun (typeSub i t t1) (typeSub i t t2)
----------------------------------------------------------------------------------------------

evalType :: [LocalType] -> State LocalType
evalType [x] = case x of
                 (FreeT n ts)   -> do t   <- lookForType n  
                                      evalType' (convertionType t) ts 
                 (BoundT _)  -> return x
                 (Fun t1 t2) -> do t1' <- evalType [t1]
                                   t2' <- evalType [t2]
                                   return (Fun t1' t2')
                 (Uni t)     -> do t' <- evalType [t]
                                   return (Uni t')
evalType (x:xs) = case x of
                    (FreeT n ts)  -> do t  <- lookForType n
                                        t' <- evalType ((convertionType t):ts) 
                                        evalType' t' xs
                    _          -> throwTypeApp $ (show x) ++ ". Expecting a type with a" ++ show (FreeT "..." []) ++" form"



evalType' ::  LocalType -> [LocalType] -> State LocalType
evalType' t []      = return t
evalType' (Uni t) (t':ts) = do t'' <- evalType' (typeSub 0 t' t) ts
                               return t''
evalType' t _             = throwTypeApp $ (show t) ++ ".Expecting a "++ show(Uni t)


----------------------------------------------------------

instanceTerm :: Int -> LocalTerm -> LocalType -> State LocalTerm
instanceTerm i (FreeV s) t   = do v <- lookForExp s
                                  instanceTerm i (quote v) t
instanceTerm _ (Bound j) _   =    return (Bound j)                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
instanceTerm i (App t1 t2) t = do t1' <- instanceTerm i t1 t
                                  t2' <- instanceTerm i t2 t 
                                  return (App t1' t2')
instanceTerm i (Lam t' e) t  = do t1 <- instanceTerm i e t
                                  return (Lam (typeSub i t t') t1)
instanceTerm i (LamT  e) t   = do t1 <- instanceTerm (i+1) e t
                                  return (LamT t1)
instanceTerm i (AppT e ts) t = do t1 <- instanceTerm i e t
                                  return (AppT t1 (L.map (typeSub i t) ts))




---------------------------------------------------------------------------------
eval :: LocalTerm -> State Value

eval (FreeV s)   = do v <- lookForExp s
                      return v

eval (App t1 t2) = do v  <- eval t1
                      v' <- eval t2
                      case quote v of
                        (Lam _ t1') -> eval (expSub 0 (quote v') t1')
                        e           -> throwApp (show e)

eval (Lam  t l)  = return (VLam t l)
eval (LamT l)  = return (VType l)

eval (AppT l t)  = do v  <- eval l
                      t' <- evalType t
                      case v of 
                        (VType e) -> do e' <- instanceTerm 0 e t'
                                        eval e'
                        _         -> throwApp (show v)
                                                  

eval e           = throwApp (show e)
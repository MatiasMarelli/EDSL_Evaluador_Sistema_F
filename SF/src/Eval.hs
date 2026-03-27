module Eval where
import Common 
import Data.Map    as M


convertion :: Term -> LocalTerm

convertion = convertionExp' [] []
----------------------------------------------------------
convertionExp' :: [String] -> [String] -> Term -> LocalTerm

convertionExp' xs ts (EAbs n t l) = Lam (convertionType' ts t) (convertionExp' (n:xs) ts l)

convertionExp' xs ts (EApp t1 t2) = let (v1,v2) = (fun t1,fun t2)
                                    in App v1 v2
                                    where fun = (convertionExp' xs ts)

convertionExp' xs ts (ETAbs (VarT n) l)  = LamT t (convertionExp' xs l)

convertionExp' xs ts (ETApp l ts) = AppT (convertionExp' xs l) ts

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
convertionType' xs (UniT n t)   = Uni n (convertionType' (n:xs) t)
convertionType' xs (FunT t1 t2) = Fun (convertionType' xs t1) (convertionType' xs t2)
convertionType' xs (VarT n)     = case look xs n of
                                    Nothing -> FreeT n
                                    Just i  -> BoundT i

----------------------------------------------------------
 
expSub :: Int -> LocalTerm -> LocalTerm -> LocalTerm 
expSub i t (Bound j)  | i == j    = t
                      | otherwise = Bound j
expSub _ _ (FreeV n)              = FreeV n
expSub i t (App t1 t2)            = App (expSub i t t1) (expSub i t t2)
expSub i t (Lam t' u)             = Lam t' (expSub (i+1) t u)
expSub i t (LamT t' u)            = LamT t' (expSub i t u)
expSub i t (AppT u t')            = AppT (expSub i t u) t'


quote :: Value -> LocalTerm
quote (VLam t l)  = Lam  t l
quote (VType t l) = LamT t l

--REESCRIBIR LINDO PORFA
typeSub :: Type -> String -> LocalTerm -> LocalTerm

typeSub _ _ (FreeV v)           = FreeV v 
typeSub t n (App t1 t2)         = App (typeSub t n t1) (typeSub t n t2)
typeSub t n (Lam t1 e)          = Lam (typeSub' t n t1) (typeSub t n e)
typeSub t n (LamT t1 e)         = LamT t1 (typeSub t n e)
typeSub t n (AppT e t1)         = AppT (typeSub t n e) t1 
typeSub _ _ (Bound i)           = Bound i


typeSub' :: Type -> String -> Type -> Type
typeSub' t n (VarT s) | s == n    = t
                      | otherwise = VarT s
typeSub' t n (FunT t1 t2)         = FunT (f t1) (f t2)
                                  where f = (typeSub' t n)
typeSub' t n (UniT tn t1)         | not (n == tn)   = UniT tn (typeSub' t n t1)
                                  | otherwise       = UniT tn t1
    
evalType :: [Type] -> State Type
evalType [x]    = case x of
                  (VarT n) -> lookForType n
                  _        -> throwTypeDef
                  
evalType (x:xs) = case x of
                  (VarT n) -> do t' <- lookForType n 
                                 evalType' t' xs
                  _        -> throwTypeDef


evalType []     = throwTypeDef


evalType' :: Type -> [Type] -> State Type
evalType' (UniT n t) [x]    = return (typeSub' x n t)  
evalType' (UniT n t) (x:xs) = let t' = typeSub' x n t
                              in evalType' t' xs                              

evalType' _ _               = throwTypeDef
----------------------------------------------------------




---------------------------------------------------------------------------------
eval :: LocalTerm -> State Value

eval (FreeV s)   = do v <- lookForExp s
                      return v

eval (App t1 t2) = do v  <- eval t1
                      v' <- eval t2
                      case quote v of
                        (Lam _ t1') -> eval (expSub 0 (quote v') t1')
                        _           -> throwApp

eval (Lam t l)   = return (VLam t l)
eval (LamT t l)  = return (VType t l)

eval (AppT l t)  = do v  <- eval l
                      t' <- evalType t
                      case v of 
                        (VType (VarT n) e) -> eval (typeSub t' n e)
                        _                  -> throwApp
                                                  

eval _           = throwApp
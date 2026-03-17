module Eval where
import Common 
import Data.Maybe
import Data.Map


convertion :: Term -> LocalTerm

convertion = convertion' []
----------------------------------------------------------
convertion' :: [String] -> Term -> LocalTerm

convertion' xs (EAbs n t l) = Lam t (convertion' (n:xs) l)

convertion' xs (EApp t1 t2) = let (v1,v2) = (fun t1,fun t2)
                              in App v1 v2
                              where fun = (convertion' xs)

convertion' xs (ETAbs t l)  = LamT t (convertion' xs l)

convertion' xs (ETApp l t)  = let v = convertion' xs l
                              in AppT v t

convertion' xs (EVar n)     = case look xs n of
                              Nothing -> FreeV n
                              Just i -> Bound i

----------------------------------------------------------

look :: [String] -> String -> Maybe Int
look = findBound 0 

findBound :: Int -> [String] -> String -> Maybe Int
findBound _ [] _ = Nothing
findBound i (x:xs) n = if x == n then Just i
                       else findBound (i+1) xs n



----------------------------------------------------------
 
expSub :: Int -> LocalTerm -> LocalTerm -> LocalTerm 
expSub i t (Bound j)  | i == j    = t
                      | otherwise = Bound j
expSub _ _ (Bound j)              = Bound j
expSub _ _ (FreeV n)              = FreeV n
expSub i t (App t1 t2)            = App (expSub i t t1) (expSub i t t2)
expSub i t (Lam t' u)             = Lam t' (expSub (i+1) t u)
expSub i t (LamT t' u)            = LamT t' (expSub i t u)
expSub i t (AppT u t')            = AppT (expSub i t u) t'

typeSub :: LocalTerm -> Type  -> LocalTerm
typeSub (Bound i) _ = Bound i
typeSub (FreeV s) _ = FreeV s
typeSub (App t1 t2 ) n = App (typeSub t1 n) (typeSub t2 n)
typeSub (Lam t u) n = Lam (typeSub' t n) (typeSub u n)
typeSub (LamT t u ) n = LamT t (typeSub u n)
typeSub (AppT u t) n = AppT (typeSub u n) t

typeSub' :: Type -> Type -> Type

quote :: Value -> Term
quote (VLam t u)  = Lam t u
quote (VType t u) = LamT t u
----------------------------------------------------------

lookForExp :: String -> ExpEnv -> Maybe Value
lookForExp _ []              = Nothing
lookForExp n ((n',(v,t)):xs) = if n == n' then Just v
                                          else lookForExp n xs  


lookForType :: String -> TypeEnv -> Maybe Type
lookForType _ []                = Nothing
lookForType n ((n',(_ , t)):xs) = if n == n' then Just t
                                             else lookForType n xs

eval :: ExpEnv -> TypeEnv -> LocalTerm -> Value
eval ee te (FreeV n)   = case lookForExp n ee of
                         Just v -> v
eval ee te (LamT t e) = VType (VarT t) e

eval ee te (App t1 t2) = let (Lam t t3) = (eval ee te t1)
                         in eval ee te (expSub 0 t2 t3)


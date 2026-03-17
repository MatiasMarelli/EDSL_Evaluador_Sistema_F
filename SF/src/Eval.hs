module Eval where
import Common 
import Data.Maybe
import Data.Map    as M


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


----------------------------------------------------------



lookForExp :: ExpVar ->State (Maybe Value)
lookForExp n = State (\e -> (look' n e,e))
               where look' n e  = case M.lookup n e of
                                  Nothing -> Nothing
                                  Just (Right _) -> Nothing
                                  Just (Left (v',_)) ->Just v'

updateExpEnv :: ExpVar -> Value -> Type -> State ()
updateExpEnv n v t = State(\e -> ((),M.insert n (Left (v,t)) e))




--eval :: LocalTerm -> Value
--eval (FreeV x) = 


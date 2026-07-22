module TypeCheck  where
import Common hiding (err)
import Prelude hiding (return,(>>=))
import Data.Map as M  (lookup)
import Data.List as L (map,elem)
import Eval (evalType, convertionType,convertion,typeSub)
import PrettyPrinter 
import Text.PrettyPrint.HughesPJ (render)

--Un AST donde no se pierde referencia de nombres de Bindeos

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



----------------------------------------------------------------------------------
--Definimos una estructura para poder llevar el Tipo inferido y en caso que sea necesario acarrea un error
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
---------------------------------------------------------------------------------
-- Convertimos Terminos en Terminos para el Checkeo
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


-- Convertimos Tipos en Tipos para el Checkeo
type2CheckType :: Type -> CheckerType
type2CheckType = type2CheckType' [] 

type2CheckType' :: [String] -> Type -> CheckerType
type2CheckType' c (UniT v t)   = CUni v (type2CheckType' (v:c) t)
type2CheckType' c (VarT n ls)  | L.elem n c  = CBound n
                               | otherwise = CDef n (L.map (type2CheckType' c) ls)
type2CheckType' c (FunT t1 t2) = CFun (f t1) (f t2)
                                 where f = type2CheckType' c 

--Buscamos en el State que tengamos en ese momento
lookContext :: String -> [(String,CheckerType)] -> Maybe CheckerType
lookContext _ []     = Nothing
lookContext n (x:xs) | n == (fst x) = Just $ snd x  
                     | otherwise    = lookContext n xs

--Un map para el Bind (>>=)
map' :: String -> CheckerType -> [CheckerType] -> TypeChecker [CheckerType]
map' _ _ []     = return []
map' n t (x:xs) = subType'' n t x >>= (\t' -> map' n t xs >>= \ts -> return (t': ts))

--Reducimos los tipos luego de un Instance
instanceType :: CheckerType -> [CheckerType] -> TypeChecker CheckerType
instanceType t []               = return t
instanceType (CUni n t') (t:ts) = subType'' n t t' >>= \x -> instanceType x ts  
instanceType _ _                = err $ "Expecting a forall for instance"

--Aplicamos sustituciones y quitamos los Forall
subType :: CheckerTerm -> [CheckerType] -> TypeChecker CheckerTerm
subType l []                        = return l
subType (CTAbs (CBound n) l) (t:ts) = subType' n t l >>= \t' -> subType t' ts
subType l _                         = err $ "Bad Instantiation, more Types than Universal in: "++ showterm l

--Recorremos el Term para sustituir
subType' :: String -> CheckerType -> CheckerTerm -> TypeChecker CheckerTerm
subType' _ _ (CVar n)              = return $ CVar n
subType' n t (CAbs n' t' l)        = subType'' n t t' >>= (\x ->
                                     subType' n t l   >>= (\y -> return $ CAbs n' x y))  
subType' n t (CApp l1 l2)          = subType' n t l1  >>= (\x ->
                                     subType' n t l2  >>= (\y -> return $ CApp x y ))
subType' n t (CTAbs (CBound n') l) | n == n'   = return $ CTAbs (CBound n') l
                                   | otherwise = subType' n t l >>= \x -> return $ CTAbs (CBound n') x
subType' n t (CTApp l xs)          = map' n t xs >>= (\ts -> subType' n t l >>= \l' -> return $ CTApp l' ts)

--Sustituimos dentro de los tipos
subType'' :: String -> CheckerType -> CheckerType -> TypeChecker CheckerType
subType'' n t (CDef n' xs)             = map' n t xs >>= \ts -> return $ CDef n' ts
subType'' n t (CBound n')  | n' == n   = return t
                           | otherwise = return (CBound n')
subType'' n t (CUni n' t') | n' == n   = return $ CUni n' t'
                           | otherwise = subType'' n t t' >>= \t'' -> return $ CUni n' t'' 
subType'' n t (CFun t1 t2 )            = subType'' n t t1 >>= \t1' -> subType'' n t t2 >>= \t2' -> return $ CFun t1' t2'

showtype :: CheckerType -> String
showtype = render.printType. convertionType. checkerType2Type 
showterm :: CheckerTerm -> String
showterm = render. printTerm. convertion. checkerTerm2Term

-- Funcion que infiere el tipo de un Term para
-- Verificar que este bien tipado
-- Y comprobar que los tipos matcheen con lo dado por su definicion
-- Se tiene un Env con definiciones previas para variables Libres
infer :: Env -> CheckerTerm -> TypeChecker CheckerType
infer e l = infer' e [] l 

-- Lleva una lista de contexto con las variables ligadas y su tipo
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
                                              else err $ "Expected a " ++ showtype t1' ++ " but receive a "++showtype t2 ++ " in " ++ showterm l1 
                             _             -> err $ "Expected a Function but receive: " ++ showtype t1 ++ " in " ++ showterm a))
infer' e c (CTAbs t l)  = case t of 
                          (CBound n)  -> infer' e c l >>= \t' -> return $ CUni n t'
                          _           -> err $ "Not expected a: "++ showtype t ++ " next to TypeAbstraction"
infer' e c (CTApp l ts) =  infer' e c l >>= \t' -> case t' of 
                                                     CDef n xs -> case M.lookup n e of 
                                                                    Just(Right t) -> instanceType (type2CheckType t) (xs ++ ts)
                                                                    _             -> err $ "Variable "++ (show n ) ++ " not defined"
                                                     CUni _ _  -> instanceType t' ts
                                                     _         -> err $ "Bad instantiation didnt expect "++ showtype t'                     



-- Caso borde donde la funcion definida es un constructor de tipo o elemento base del mismo
-- Ya que estos se definen por compresnion
-- Ver como ejemplos en Defs.lam (nil , cons, zero, suc, etc)
isConstructor :: Env -> CheckerType -> TypeChecker CheckerType
isConstructor e (CFun t' t) = isConstructor e t >>= \t'' -> return $ CFun t' t'' 
isConstructor e (CDef n xs) = case M.lookup n e of 
                                Just(Right t) -> instanceType (type2CheckType t) xs
                                _             -> err $ "Variable "++ (show n ) ++ " not defined"
isConstructor e (CUni n t)  = isConstructor e t >>= \t' -> return (CUni n t')
isConstructor _ t           = return t


-- Funcion auxiliar para reconstruir el tipo original
-- Se usa para poder luego de inferir poder mostrar el resultado
checkerType2Type :: CheckerType -> Type 
checkerType2Type (CDef n xs)   = VarT n (L.map checkerType2Type xs)
checkerType2Type (CBound n)    = VarT n []
checkerType2Type (CUni n t)    = UniT n (checkerType2Type t)
checkerType2Type (CFun t1 t2 ) = FunT (checkerType2Type t1) (checkerType2Type t2) 

checkerTerm2Term :: CheckerTerm -> Term
checkerTerm2Term (CVar n)     = EVar n
checkerTerm2Term (CAbs n t l) = EAbs n (checkerType2Type t) (checkerTerm2Term l)
checkerTerm2Term (CApp l1 l2) = EApp (checkerTerm2Term l1) (checkerTerm2Term l2)
checkerTerm2Term (CTAbs t l)  = ETAbs (checkerType2Type t) (checkerTerm2Term l)
checkerTerm2Term (CTApp l ts) = ETApp (checkerTerm2Term l) (L.map checkerType2Type ts)
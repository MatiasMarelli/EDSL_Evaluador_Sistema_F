module File (compileFile) where
import Prelude hiding ((>>=))
import Common hiding (err)
import System.IO
import Parse
import TypeCheck (convertionT,type2CheckType,infer,TypeChecker,CheckerTerm,CheckerType,err,(>>=),isConstructor,showtype)
import Eval
import Data.Maybe
import Data.Map as M
import Control.Monad.IO.Class
import PrettyPrinter 

-- Funcion que checkea que el Tipo escrito coincida con lo que se infiere de su definicion.
assertType :: Type -> Term -> Env -> TypeChecker CheckerType
assertType t' l' e = let (t,l) = (type2CheckType t',convertionT l')
                     in infer e l >>= \t'' -> if t'' == t then (Right t)
                                                          else  isConstructor e t >>= \v ->  -- Caso donde la definicion sea un constructor 
                                                            if v == t'' then (Right t)       -- En estos casos el tipo construido se hace por "comprension"
                                                                        else err $ showtype t ++ "is diferent than infered :"  ++ showtype t''

-- Conversion para que se transforme la definicion de tipo a una forma que la maquina entienda
-- sin modificar como nosotros lo solemos hacer en papel
convertDefT :: Defs -> Type
convertDefT (DefT _ xs t) = convertDefT' xs t

convertDefT':: [Type] -> Type -> Type
convertDefT' [] t     = t
convertDefT' ((VarT n []): ts) t = UniT n (convertDefT' ts t) 

                  
--Funcion que verifica que el parser no tuvo problemas en todo el archivo
checkParse :: [Either [Defs] String] -> Maybe String
checkParse [] = Nothing
checkParse (x:xs) = case x of
                      (Right e) -> Just e 
                      (Left d)  -> checkParse xs 

--No queremos hacer case dentro de funciones
--Solamente se ejecuta en casos donde seguro no hay un Right
unLeft :: Either a b -> a
unLeft (Left x) = x


--Leemos todo lo definido en el archivo, si todo es correcto, se devuelve el estado con toda la informacion recolectada
parseFile :: [[Defs]] -> State Env
parseFile xs = do e <- aux (concat xs)
                  case e of 
                    [] -> do s <- getState
                             return s
                    _  -> do throwExpDef (show e)
                    
                       
--Verificamos que el orden del archivo sea correcto, es decir que se le de un tipo antes de una Definicion
aux :: [Defs] -> State [Defs]
aux []       = return []
aux [x]      = case x of
                (DefT (VarT n []) ts t) -> do updateTypeEnv n (convertDefT x)
                                              return []
                _                       -> throwCompile (show x )
aux (x:y:xs) = case x of
                 (Def _ _)            -> throwCompile $ " Expecting a type for expression first, instead receive a: "++ show x
                 (DefT (VarT n []) ts t) -> do updateTypeEnv n (convertDefT x)
                                               aux (y:xs)
                 (TypeExp n t)        -> case y of
                                          (Def n' e) -> (if n == n' then do s <- getState
                                                                            case assertType t e s of 
                                                                              Left err -> throwCompile $ err ++ " in definition: "++ show (n)
                                                                              Right _  -> do v <- eval (convertion e)
                                                                                             updateExpEnv n v t
                                                                                             aux xs
                                                                    else throwCompile $ " Expecting a definition for: "++ n ++ " but receive one for "++ n' )

                                                                    
-- Modifica el nombre de un archivo para ponerlo en el path correspondiente
fileName :: String -> String
fileName n = "Files/" ++ n ++".lam"


-- Funcion que compila un archivo de definiciones y devuelve el estado resultante.
compileFile :: String -> IO Env
compileFile n = do s <- liftIO (readFile (fileName n))
                   let xs = Prelude.map (getResult . defPar) (lines s)
                   case checkParse xs of 
                     Nothing -> (let e = runState (parseFile (Prelude.map unLeft xs)) M.empty in
                                 case e of
                                   Left err       -> (do putStrLn (show err)
                                                         return M.empty)
                                   Right (v,s) -> return v)
                     Just e'  -> do putStrLn e'
                                    return M.empty
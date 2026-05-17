module File where

import Common hiding (err)
import System.IO
import Parse
import TypeCheck (TypeChecker,checkType,appType)
import Eval
import Data.Maybe
import Data.Map as M
import Control.Monad.IO.Class

prelude :: String
prelude = "src/Defs.lam"


assertType :: Type -> Term -> Env -> TypeChecker LocalType
assertType t e s = let t'      = convertionType t 
                       expType = (checkType s (convertion e))
                   in case expType of
                        Left err     -> Left $ err ++ " in " ++ show (convertion e)
                        Right t'' -> if t' == t'' then Right t'' 
                                                    else case appType s [t'] of
                                                           Left x    -> Left x 
                                                           Right t''' -> if t''' == t'' then Right t''
                                                                                        else Left $ "No matching type. Expecting a " ++ show(t''') ++ " but receive a " ++ show(t'') ++ " in  " ++ show (t') ++ " and in " ++ show(t'')



convertDefT :: Defs -> Type
convertDefT (DefT _ xs t) = convertDefT' xs t

convertDefT':: [Type] -> Type -> Type
convertDefT' [] t     = t
convertDefT' ((VarT n []): ts) t = UniT n (convertDefT' ts t) 

compileFiles :: IO Env
compileFiles = do s <- liftIO (readFile prelude)
                  let xs = Prelude.map (getResult . defPar) (lines s)
                  case checkParse xs of 
                    Nothing -> (let e = runState (parseFile (Prelude.map unLeft xs)) M.empty in
                                case e of
                                  Left err       -> (do putStrLn (show err)
                                                        return M.empty)
                                  Right (v,s) -> return v)
                    Just e'  -> do putStrLn e'
                                   return M.empty
                  

checkParse :: [Either [Defs] String] -> Maybe String
checkParse [] = Nothing
checkParse (x:xs) = case x of
                      (Right e) -> Just e 
                      (Left d)  -> checkParse xs 

unLeft :: Either a b -> a
unLeft (Left x) = x



parseFile :: [[Defs]] -> State Env
parseFile xs = do e <- aux (concat xs)
                  case e of 
                    [] -> do s <- getState
                             return s
                    _  -> do throwExpDef (show e)
                    
                       

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
                                                                              Left err -> throwCompile $ "not matching types, "++ err ++ " checking "++ show (n)
                                                                              Right _  -> do v <- eval (convertion e)
                                                                                             updateExpEnv n v t
                                                                                             aux xs
                                                                    else throwCompile $ " Expecting a definition for: "++ n ++ " but receive one for "++ n' )


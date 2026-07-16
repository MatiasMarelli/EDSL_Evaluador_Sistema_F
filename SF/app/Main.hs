module Main (main) where
import Eval
import Common
import Data.Map as M
import Parse
import Data.Maybe
import PrettyPrinter
import Text.PrettyPrint.HughesPJ      ( render, text)
import TypeCheck hiding (return,showterm,showtype)
import File 
import System.IO
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import System.Console.Haskeline

emptyState :: Env
emptyState = M.empty

data Command = ClearState | Compile String |Eval String | Type String | PrintState | PrintExpression String |AST String| Browse String |Help | Unknown | Quit
input ::String
input = "SF>"

main :: IO ()
main =  runInputT defaultSettings (loop emptyState)


loop :: Env ->  InputT IO ()
loop e = do comm <- getInputLine input
            case comm of 
              Just c -> do e' <- handleComm e c
                           case e' of 
                             Just s -> loop s
                             _      -> return ()
              _      -> return ()

isSpace :: Char -> Bool
isSpace ' ' = True
isSpace _   = False

handleComm :: Env -> String -> InputT IO (Maybe Env)
handleComm e c = do c' <- lift $ handleComm' c 
                    case c' of 
                      Quit                -> return Nothing
                      Unknown             -> do lift $ putStrLn "Unknown command try :h for help"
                                                return $ Just e
                      Help                -> do lift $ helpComm
                                                return $ Just e 
                      ClearState          -> return $ Just emptyState
                      PrintState          -> do lift $ showState e
                                                return $ Just e
                      Compile file        -> do e' <- lift $ (compileFile file)
                                                lift $ putStrLn $ "Variables loaded:" ++ show(M.size e')
                                                return $ Just $ M.union e' e 
                      PrintExpression exp -> do lift $ printExp e exp
                                                return $ Just e
                      AST exp             -> do lift $ printAST exp
                                                return $ Just e
                      Eval exp            -> do lift $ evaluate e exp
                                                return $ Just e
                      Type exp            -> do lift $ showtype e exp
                                                return $ Just e
                      Browse exp          -> let s = reverse (dropWhile isSpace (reverse (dropWhile isSpace exp)))
                                             in case M.lookup s e of 
                                                  Nothing           -> do lift $ putStrLn "Variable not found"
                                                                          return $ Just e
                                                  Just (Left (v,_)) -> do lift $ putStrLn $ render $ printTerm (quote v)
                                                                          return $ Just e 
                                                  Just (Right t)    -> do lift $ putStrLn $ render $ printType (convertionType t)
                                                                          return $ Just e
                                                

                      

                      
handleComm' :: String -> IO (Command)
handleComm' comm = case words comm of 
                        ([":h"])        -> return (Help)
                        (":c":rest)     -> return (Compile $ unwords rest)
                        (":p":rest)     -> return (PrintExpression $ unwords rest)
                        ([":clear"])    -> return ClearState  
                        ([":s"])        -> return PrintState 
                        (":a":rest)     -> return (AST $ unwords rest)
                        ([":q"])        -> return Quit
                        (":e": rest)    -> return $ Eval $ unwords rest
                        (":t": rest)    -> return $ Type $ unwords rest 
                        (":b": rest)    -> return (Browse $ unwords rest)
                        _               -> return (Unknown)



helpComm :: IO ()
helpComm   =  do putStrLn "List of commands:"
                 putStrLn ":c  <filename(.lam)>  >> Compiles the file and update the global state, matching variables with current state will be replaced with new file ones"
                 putStrLn ":e  <expression>      >> Evaluate an expression"
                 putStrLn ":t  <expression>      >> Prints the infered type of the given expression"
                 putStrLn ":b  <variable>        >> Shows value or type of a defined variable"
                 putStrLn ":p  <expression>      >> Prints the expression, if its defined in state it will prints the value associated"
                 putStrLn ":clear                >> Clears current global state"
                 putStrLn ":s                    >> Prints current global state, in other words all defined variables"
                 putStrLn ":a  <expression>      >> Prints the AST for the expression"
                 putStrLn ":q                    >> Exit the program"
                 putStrLn ":h                    >> Prints this message"
                 return ()
                    


showState :: Env -> IO ()
showState s = let keys = M.keys s 
              in do putStrLn $ show keys
                    return ()

printAST :: String -> IO ()
printAST exp = case expPar exp of 
                 Failed e  -> do putStrLn e 
                                 return ()
                 Ok p      -> do putStrLn $ show p
                                 return ()

evaluate :: Env -> String -> IO ()
evaluate e s = case expPar s of 
                 Failed e  -> do putStrLn e 
                                 return ()
                 Ok a      -> case infer e (convertionT a) of 
                                   Right _ -> case runState (eval $ convertion a) e of  
                                                Left e     -> do putStrLn e 
                                                                 return () 
                                                Right(v,_) -> do putStrLn $ render (printTerm $ quote v)
                                                                 return ()
                                   Left e -> do putStrLn e 
                                                return ()
printExp :: Env -> String -> IO ()
printExp e s = case expPar s of 
                 Failed e  -> do putStrLn e
                                 return ()
                 Ok a      -> let t = convertion a 
                              in case t of 
                                  FreeV n -> case M.lookup n e of 
                                               Just (Left (v,_)) -> do putStrLn $ render $ printTerm (quote v)
                                                                       return ()
                                               _                 -> do putStrLn $ render $ printTerm t
                                                                       return ()
                                  _       -> do putStrLn $ render $ printTerm t
                                                return ()

showtype :: Env -> String -> IO()
showtype e s = case expPar s of 
                 Failed e -> do putStrLn e 
                                return ()
                 
                 Ok a     -> let x = convertionT a
                             in case infer e x of 
                                  Left err -> do putStrLn err
                                                 return ()

                                  Right t  -> do putStrLn $ render $ printType $ convertionType $ checkerType2Type t
                                                 return ()   
module Main (main) where
import Eval
import Common
import Data.Map as M
import Parse
import Data.Maybe
import PrettyPrinter
import TypeCheck hiding (return)
import File (compileFiles,unLeft)


test :: String
test = " tototo " 
    --"((/\\ X. \\x: X -> X . x) <Nat>) ((/\\Y./\\Z. \\y:Y -> Z -> Y. y y) <List> ) "


-- List X = \/ R . (X -> R -> R) -> R -> R
-- List = \/ X . \/R . (X -> R -> R) -> R -> R ||| <List Nat> = <List> <Nat> 
e:: IO ()
e = do env <- compileFiles
       let v  = runState (eval (convertion (unLeft (getResult (expPar test))))) env
       case v of 
        Left e -> return ()
        Right (t,_) -> do let v' = printTerm(quote(t))
                          putStrLn $ show (v')
                          return ()
   
main :: IO ()
main = undefined

ej :: IO (TypeChecker CheckerType)
ej = do v <- compileFiles 
        let t = infer v (convertionT(unLeft (getResult (expPar "tototo"))))
        return t 
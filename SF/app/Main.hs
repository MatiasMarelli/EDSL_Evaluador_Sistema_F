module Main (main) where
import Eval
import Common
import Data.Map as M
import Parse
import Data.Maybe
import TypeCheck (checkType)
import File (compileFiles,unLeft)


test :: String
test = "(/\\X. /\\Y. \\x:X -> Y -> Y. \\b: X -> Y . x b) <List Nat>" 
    --"((/\\ X. \\x: X -> X . x) <Nat>) ((/\\Y./\\Z. \\y:Y -> Z -> Y. y y) <List> ) "


-- List X = \/ R . (X -> R -> R) -> R -> R
-- List = \/ X . \/R . (X -> R -> R) -> R -> R ||| <List Nat> = <List> <Nat> 
--e:: Either Error (Value,Env)
--e = runState (eval (convertion (fromJust (getResult (expPar test))))) M.empty 
   
main :: IO ()
main = undefined



ej :: IO (Either String LocalType)
ej = do v <- compileFiles
        let t = checkType v  (App (convertion (unLeft ( getResult (expPar "\\b:Int. ((if <Bool>) b) (zero <Nat>) (suc (zero <Nat>))")))) (FreeV "true")) 
        return t
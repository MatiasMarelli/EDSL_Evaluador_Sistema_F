module Main (main) where
import Eval
import Common
import Data.Map as M
import Parse
import Data.Maybe
import TypeCheck (checkType)
import File (compileFiles,unLeft)


test :: String
test = " /\\X . \\b:Bool .\\a:X. \\c:X. b <X> a c " 
    --"((/\\ X. \\x: X -> X . x) <Nat>) ((/\\Y./\\Z. \\y:Y -> Z -> Y. y y) <List> ) "


-- List X = \/ R . (X -> R -> R) -> R -> R
-- List = \/ X . \/R . (X -> R -> R) -> R -> R ||| <List Nat> = <List> <Nat> 
e:: IO (Either Error (Value,Env))
e = do env <- compileFiles
       let v = runState (eval (convertion (unLeft (getResult (expPar test))))) env
       return v
   
main :: IO ()
main = undefined



ej :: IO (Either String LocalType)
ej = do v <- compileFiles
        let t = checkType v   (convertion (unLeft ( getResult (expPar "/\\X.\\x:X.x")))) 
        return t 
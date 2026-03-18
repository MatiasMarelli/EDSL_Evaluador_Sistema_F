module Main (main) where
import Eval
import Common
import Data.Map as M
import Parse



test :: String
test = "(/\\X. /\\Y. \\x:X -> Y -> Y. \\b: X -> Y . x b) <List Nat> " 
    --"((/\\ X. \\x: X -> X . x) <Nat>) ((/\\Y./\\Z. \\y:Y -> Z -> Y. y y) <List> ) "


-- List X = \/ R . (X -> R -> R) -> R -> R
-- List = \/ X . \/R . (X -> R -> R) -> R -> R ||| <List Nat> = <List> <Nat> 
e:: Either Error (Value,Env)
e = runState (eval (convertion (getResult (expPar test)))) M.empty 
   
main :: IO ()
main = undefined

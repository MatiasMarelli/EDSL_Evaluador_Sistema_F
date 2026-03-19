{
module Parse where
import Data.Char
import Common
import Data.Maybe
}

%name par Exp
%name defParser Defs
%tokentype { Token }
%error { parseError }
%monad { M } { thenM } { returnM } 
%lexer { lexer } { TEOF }

%token 
  '\\'     { TAbs }
  ':'      { TColon }
  '.'      { TDot }
  '/\\'    { TTAbs }
  '('      { TOpenP }
  ')'      { TCloseP }
  '<'      { TOpenI }
  '>'      { TCloseI }
  '4'      { TForAll }
  '->'     { TFun }
  VAR      { TVar $$ }
  VART     { TVarT $$ }
  DEF      { TDef }
  '='      { TEquals }
  ':='     { TTEq }
  
%left  '=' ':='
%right '->'
%right '4' '/\\'
%right '\\' '.'

%% 

Exp     :: { Term }
        : Atom '<' Poli '>'          { ETApp $1 $3 }
        | '/\\' VART '.' Exp         { ETAbs (VarT $2) $4 }
        | '\\' VAR ':' Type '.' Exp  { EAbs $2 $4 $6 }	
        | NAbs                       { $1 }

NAbs    :: { Term }
	: NAbs Atom   		     { EApp $1 $2 } 
        | Atom                       { $1 }


Atom    :: { Term }
	: VAR                        { EVar $1 }
        | '(' Exp ')'                { $2 }


Type    :: { Type }
        : Type '->' Type             { FunT $1 $3 }
        | '(' Type ')'               { $2 }
        | VART                       { VarT $1 }
        | '4' VART '.' Type          { UniT $2 $4 }

Defs    : Defexp Defs                { $1 : $2 }
	|                            { [] }

--        DEF VART := Type 
Defexp  : DEF VART ':=' Type         { DefT (VarT $2) $4 }
	| DEF VAR '=' Exp            { Def  $2 $4 }

Poli    : VART Poli                  { (VarT $1) : $2 }
	|                            { [] } 
{

data Token = TAbs
           | TTAbs
           | TColon
           | TDot
           | TOpenP
           | TCloseP
           | TOpenI
           | TCloseI
           | TForAll
           | TFun
           | TVar String
           | TVarT String
	   | TEOF
           | TDef
           | TEquals
           | TTEq 
     deriving Show



data ParseResult a = Ok a | Failed String deriving (Show)

type LineNumber = Int
type M a = String -> LineNumber -> ParseResult a 

thenM :: M a -> (a -> M b) -> M b
m `thenM` k = \s l -> case m s l of
                           Ok a     -> k a s l
                           Failed e -> Failed e

returnM :: a -> M a
returnM a = \s l -> Ok a

failM :: String -> M a
failM err = \s l -> Failed err

catchM :: M a -> (String -> M a ) -> M a 
catchM m k = \s l -> case m s l of
                           Ok a     -> Ok a
                           Failed e -> k e s l

getLineNumber :: M LineNumber
getLineNumber = \s l -> Ok l

getResult :: ParseResult a -> a
getResult (Ok a) = a
getResult _ = undefined

parseError :: Token -> M a 
parseError _ = \ s i -> Failed $ "Linea "++(show (i::LineNumber))++": Error de parseo "++(s) 


lexer :: (Token -> M a ) -> M a 
lexer cont s = case s of
                    [] -> cont TEOF []
                    ('\n':s) -> \line -> lexer cont s (line + 1)
                    (c:cs)
                          | isSpace c -> lexer cont cs
                          | isAlpha c -> lexVar (c:cs)
                    ('-' : ('>':cs)) -> cont TFun cs
                    ('\\':cs) -> cont TAbs cs
                    (':':('=':cs)) -> cont TTEq cs
                    ('=': cs) -> cont TEquals cs
                    ('.': cs) -> cont TDot cs
                    (':': cs) -> cont TColon cs 
                    ('(': cs) -> cont TOpenP cs
                    (')': cs) -> cont TCloseP cs 
                    ('<': cs) -> cont TOpenI cs
                    ('>': cs) -> cont TCloseI cs
                    ('4': cs) -> cont TForAll cs
                    ('/':('\\': cs)) -> cont TTAbs cs 
                    something -> \line -> Failed ((take 15 something) ++ " in line : " ++ (show line)) 
                    where lexVar s = case span isAlpha s of
                                     ("def",rest ) -> cont TDef rest
                                     ((c:cs),rest) | isUpper c -> cont (TVarT (c:cs)) rest
                                                   | otherwise -> cont (TVar (c:cs)) rest 


expPar s  = par s 1
defPar s  = defParser s 1
}

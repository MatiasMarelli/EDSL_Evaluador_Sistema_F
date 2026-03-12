{
  module Parse where
  import Common
}

%name par
%tokentype { Token }
%error { parseError }

%token 





%% 

Exp     :: { Terms }
        : '\\' VAR ':' Type '.' Exp  { LAbs $2 $4 $6 }
        | NTAbs                      { $1 }	
        | NAbs                       { $1 }

NTAbs   :: { Terms }
	: Exp '[' Type ']'           { LTapp $1 $3 }
        | NAbs                       { $1 }

NAbs    :: { Terms }
	: NAbs Atom   		     { LApp $1 $2 }
        | 
        | Atom                       { $1 }


Atom    :: { Terms }
	: VAR                        { LVar $1 }
        | '(' Exp ')'                { $2 }


Type    :: { Type }
	: TYPEE                      { EmptyT  }
        | Type '->' Type             { FunT $1 $2 }
        | '(' Type ')'               { $2 }
        | VART                       { VarT $1 }
        | '\/' VART '.' Type         { Unit $2 $ 4 }


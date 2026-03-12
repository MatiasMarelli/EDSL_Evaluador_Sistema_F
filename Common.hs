module Common where





data Type = EmptyT
          | VarT String
          | UniT String Type 
          | FunT Type Type 
          deriving (Show, Eq)  

data Terms   = LVar String
             | LAbs String Type Terms 
             | LApp Terms Terms
             | LTAbs String Terms
             | LTapp Terms Type 
             deriving (Show, Eq)

data LocalTerms = Bound Int
                | FreeV String 
                | App LocalTerms LocalTerms
                | Lam Type LocalTerms
                | FreeT String LocalTerms
                | TApp LocalTerms String 
            deriving (Show, Eq)

data Values = VLam Type LocalTerms 
            | VType Type LocalTerms
            deriving (Show, Eq)

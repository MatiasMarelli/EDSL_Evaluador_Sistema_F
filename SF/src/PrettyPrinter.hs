module PrettyPrinter
  ( printTerm  ,     
    printType        
  )
where

import  Common
import  Text.PrettyPrint.HughesPJ
import  Prelude hiding ((<>))

varsTerm :: [String]
varsTerm =
  [ c : n
  | n <- "" : map show [(1 :: Integer) ..]
  , c <- ['x', 'y', 'z'] ++ ['a' .. 'w']
  ]

varsType :: [String]
varsType = 
  [ c : n
  | n <- "" : map show [(1 :: Integer) ..]
  , c <- ['X', 'Y', 'Z'] ++ ['A' .. 'W']
  ]

parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id


pp :: Int -> Int -> [String] -> [String] -> LocalTerm -> Doc
pp iv it fv ft (Bound j)     = text (fv !! (iv-j-1))
pp _  _  _ _ (FreeV n)       = text n

pp iv it fv ft (App l1 l2)      = sep
  [
    parensIf (isLam l1) (pp iv it fv ft l1),
    nest 1 (parensIf (isLam l2 || isApp l2) (pp iv it fv ft l2))
  ]

pp iv it fv ft (Lam t l) = 
  text "\\" 
  <> text (fv !! iv)
  <> text ":"
  <> printType it ft t
  <> text ". "
  <> pp (iv+1) it fv ft l

pp iv it fv ft (LamT l) =
  text "/\\"
  <> text (ft !! it)
  <> text ". "
  <> pp iv (it +1) fv ft l

pp iv it fv ft (AppT l xs) =
  pp iv it fv ft l 
  <> text " <"
  <> sep (foldr (:) [] (map (printType it ft) xs))
  <> text ">"

isLam :: LocalTerm -> Bool
isLam (Lam _ _) = True
isLam _         = False

isApp :: LocalTerm -> Bool
isApp (App _ _) = True
isApp _         = False

isFun :: LocalType -> Bool
isFun (Fun _ _) = True
isFun _         = False

isUni :: LocalType -> Bool
isUni (Uni _)   = True
isUni _         = False


printType :: Int -> [String] -> LocalType -> Doc
printType it ft (BoundT k)   = text (ft !! (it-k-1))
printType it ft (FreeT n xs) = text n <>  if (null xs) then sep (map (printType it ft) xs) else parens (sep (map (printType it ft) xs))

printType it ft (Uni t) =
  text "\\/"
  <> text (ft !! it)
  <> text ". "
  <> printType (it +1) ft t

printType it ft (Fun t t') = 
  hsep [parensIf (isFun t || isUni t) (printType it ft t), text "->", printType it ft t']

freeTerms :: LocalTerm -> [String]
freeTerms (Bound _ )  = []
freeTerms (FreeV n)   = [n]
freeTerms (App l1 l2) = freeTerms l1 ++ freeTerms l2
freeTerms (Lam _ l)   = freeTerms l
freeTerms (LamT l)    = freeTerms l
freeTerms (AppT l _)  = freeTerms l

freeTypes :: LocalTerm -> [String]
freeTypes (Bound _ )  = []
freeTypes (FreeV n)   = []
freeTypes (App l1 l2) = freeTypes l1 ++ freeTypes l2
freeTypes (Lam t l)   = freeTypes' t ++ freeTypes l
freeTypes (LamT l)    = freeTerms l
freeTypes (AppT l xs)  = freeTerms l ++ (foldr (++) [] (map freeTypes' xs))

freeTypes' :: LocalType -> [String]
freeTypes' (BoundT _)  = []
freeTypes' (FreeT n _) = [n]
freeTypes' (Uni l)     = freeTypes' l
freeTypes' (Fun t1 t2) = (freeTypes' t1 ) ++ (freeTypes' t2)

printTerm :: LocalTerm -> Doc
printTerm t = pp 0 0 (filter (\v -> not $ elem v (freeTerms t)) varsTerm) (filter (\v -> not $ elem v (freeTypes t)) varsType) t
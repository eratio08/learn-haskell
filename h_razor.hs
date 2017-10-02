module HuttonsRazor
    (
    ) where

data Expr
  = Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval (Lit i) = i
eval (Add exp1 exp2) = (eval exp1) + (eval exp2)

printExpr :: Expr -> String
printExpr (Lit a) = show a
printExpr (Add a b) = printExpr a ++ " + " ++ printExpr b

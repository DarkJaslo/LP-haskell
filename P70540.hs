-- Expressions

data Expr = Val Int | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr

eval1 :: Expr -> Int
eval1 (Val a) = a
eval1 (Add a b) = (eval1 a) + (eval1 b)
eval1 (Sub a b) = (eval1 a)-(eval1 b)
eval1 (Mul a b) = (eval1 a)*(eval1 b)
eval1 (Div a b) = div (eval1 a) (eval1 b)
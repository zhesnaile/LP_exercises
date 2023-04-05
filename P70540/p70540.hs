data Expr = Val Int | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr

eval1 :: Expr -> Int
eval1 (Val x) = x
eval1 (Add x y) = eval1 x + eval1 y
eval1 (Sub x y) = eval1 x - eval1 y
eval1 (Mul x y) = eval1 x * eval1 y
eval1 (Div x y) = eval1 x `div` eval1 y

main::IO()
main = do
    print $ eval1 (Val 2)
    print $ eval1 (Add (Val 2) (Val 3))
    print $ eval1 (Sub (Val 2) (Val 3))
    print $ eval1 (Div (Val 4) (Val 2))
    print $ eval1 (Mul (Add (Val 2) (Val 3)) (Sub (Val 2) (Val 3)))
data Expr = Val Int | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr


eval1 :: Expr -> Int
eval1 (Val x) = x
eval1 (Add x y) = eval1 x + eval1 y
eval1 (Sub x y) = eval1 x - eval1 y
eval1 (Mul x y) = eval1 x * eval1 y
eval1 (Div x y) = eval1 x `div` eval1 y


eval2 :: Expr -> Maybe Int
eval2 (Val x) = Just x
eval2 (Add x y) = do
    x <- eval2 x
    y <- eval2 y
    return (x + y)

eval2 (Sub x y) = do
    x <- eval2 x
    y <- eval2 y
    return (x - y)

eval2 (Mul x y) = do
    x <- eval2 x
    y <- eval2 y
    return (x * y)

eval2 (Div x y) = do
    x <- eval2 x
    y <- eval2 y
    if y == 0 then Nothing
    else return (x `div` y)


eval3 :: Expr -> Either String Int
eval3 (Val x) = Right x
eval3 (Add x y) = do
    x <- eval3 x
    y <- eval3 y
    return (x + y)

eval3 (Sub x y) = do
    x <- eval3 x
    y <- eval3 y
    return (x - y)

eval3 (Mul x y) = do
    x <- eval3 x
    y <- eval3 y
    return (x * y)

eval3 (Div x y) = do
    x <- eval3 x
    y <- eval3 y
    if y == 0 then Left "div0"
    else return (x `div` y)


main::IO()
main = do
    print $ eval1 (Val 2)
    print $ eval1 (Add (Val 2) (Val 3))
    print $ eval1 (Sub (Val 2) (Val 3))
    print $ eval1 (Div (Val 4) (Val 2))
    print $ eval1 (Mul (Add (Val 2) (Val 3)) (Sub (Val 2) (Val 3)))
    print $ eval2 (Val 2)
    print $ eval2 (Add (Val 2) (Val 3))
    print $ eval2 (Sub (Val 2) (Val 3))
    print $ eval2 (Div (Val 4) (Val 2))
    print $ eval2 (Mul (Add (Val 2) (Val 3)) (Sub (Val 2) (Val 3)))
    print $ eval2 (Div (Val 4) (Val 0))
    print $ eval2 (Add (Div (Val 4) (Val 0)) (Val 3))
    print $ eval2 (Add (Val 3) (Div (Val 4) (Val 0)))
    print $ eval3 (Val 2)
    print $ eval3 (Add (Val 2) (Val 3))
    print $ eval3 (Sub (Val 2) (Val 3))
    print $ eval3 (Div (Val 4) (Val 2))
    print $ eval3 (Mul (Add (Val 2) (Val 3)) (Sub (Val 2) (Val 3)))
    print $ eval3 (Div (Val 4) (Val 0))
    print $ eval3 (Add (Div (Val 4) (Val 0)) (Val 3))
    print $ eval3 (Add (Val 3) (Div (Val 4) (Val 0)))
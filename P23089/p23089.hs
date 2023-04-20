myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f seed =
    case f seed of
        Just (m, n) -> m : myUnfoldr f n
        _ -> []

myReplicate :: a -> Int -> [a]
myReplicate x = myUnfoldr (\n -> if n == 0 then Nothing else Just (x, n - 1))

myIterate :: (a -> a) -> a -> [a]
myIterate f = myUnfoldr (\n -> Just (n, f n))

myMap :: (a -> b) -> [a] -> [b]
myMap f = myUnfoldr (\ls -> case ls of
        [] -> Nothing
        (x:xs) -> Just (f x, xs))

data Bst a = Empty | Node a (Bst a) (Bst a)

instance Show a => Show (Bst a) where
    show Empty = "."
    show (Node x l r) = "(" ++ show x ++ " " ++ show l ++ " " ++ show r ++ ")"

add :: Ord a => a -> (Bst a) -> (Bst a)
add x Empty = Node x Empty Empty
add x (Node y l r)
    | x < y          = Node y (add x l) r
    | x > y          = Node y l (add x r)
    | otherwise = Node y l r


adder :: Ord a => (Bst a, [a]) -> Maybe (Bst a, (Bst a, [a]))
adder (_, []) = Nothing
adder (tree, x:xs) = Just (add x tree, (add x tree, xs))

main::IO()
main = do
    print $ myUnfoldr (\x -> if x == 0 then Nothing else Just (x, x - 1)) 5
    print $ myReplicate 7 4
    print $ myReplicate '*' 4
    print $ take 8 $ myIterate (*2) 1
    print $ take 4 $ myIterate ('*' :) ""
    print $ myMap (*2) [1..10]
    print $ take 4 $ myMap even [1..]
    print $ show (Empty :: Bst Int)
    print $ show $ add 30 Empty
    print $ show $ add 20 $ add 10 $ add 50 $ add 30 Empty
    print $ myUnfoldr adder (Empty, [3, 1, 4, 5])
{--}

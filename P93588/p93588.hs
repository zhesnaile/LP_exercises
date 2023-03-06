myMap::(a->b)-> [a] -> [b]
myMap f l = [f x | x <- l]

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter condition l = [ x | x <- l, condition x]

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith op ll rl = [op x y | (x, y) <- zip ll rl]  

thingify :: [Int] -> [Int] -> [(Int, Int)]
thingify ll rl = [(x, y) | x <- ll, y <- rl, mod x y == 0]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], mod n x == 0]

main::IO()
main = do
    print $ myMap (*2) [1..5]
    print $ myFilter odd [1..5]
    print $ myZipWith (*) [1..4] [1..4]
    print $ thingify [1..6] [1..3]
    print $ factors 24
{---}
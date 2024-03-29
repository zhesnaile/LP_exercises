myLength :: [Int] -> Int
myLength [] = 0
myLength(_:xs) = 1 + myLength xs

myMaximum :: [Int] -> Int
myMaximum [x] = x
myMaximum (x:xs) = max x $ myMaximum xs

average :: [Int] -> Float
average ls@(x:xs) =
    let
        summary = fromIntegral $ sum ls
        len = fromIntegral $ length ls
    in
        summary / len


buildPalindrome :: [Int] -> [Int]
buildPalindrome xs = reverse xs ++ xs


remove :: [Int] -> [Int] -> [Int]
remove xs ys = [x | x <- xs, x `notElem` ys]


flatten :: [[Int]] -> [Int]
flatten [] = []
flatten [x] = x
flatten (x:xs) = x ++ flatten xs

oddsNevens :: [Int] -> ([Int],[Int])
oddsNevens xs = (odd_numbers, even_numbers)
    where
        even_numbers = filter even xs
        odd_numbers = filter odd xs


primeDivisors :: Int -> [Int]
primeDivisors m = 
    let
        isqrt :: Int -> Int
        isqrt x = floor $ sqrt (fromIntegral x :: Float)

        factors :: Int -> [Int]
        factors n  = [x | x <- [2..n], mod n x == 0]
        
        isPrime :: Int -> Bool
        isPrime n
            | n <= 1    = False
            | otherwise = null [x | x <- [2..isqrt n], mod n x == 0]
    in 
        filter isPrime $ factors m

main::IO()
main = do
    print $ myMaximum [4,3,1,5,4,5,2]
    print $ average [1,2,3]
    print $ buildPalindrome [2,4,6]
    print $ flatten [[2,6],[8,1,4],[],[1]]
    print $ remove [1,4,5,3,4,5,1,2,7,4,2] [2,4]
    print $ myLength [1,3..10]
    print $ oddsNevens [1,4,5,3,4,5,1,2,7,4,2]
    print $ primeDivisors 255
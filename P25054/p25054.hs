
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

{-
remove :: [Int] -> [Int] -> [Int] 
remove [][] = [0]

flatten :: [[Int]] -> [Int]
flatten [[]] = [0]

oddsNevens :: [Int] -> ([Int],[Int])
oddsNevens [] = ([0],[0])

primeDivisors :: Int -> [Int]
primeDivisors n = [0]
--}
main::IO()
main = do
    print $ myMaximum [4,3,1,5,4,5,2]
    print $ average [1,2,3]
    print $ buildPalindrome [2,4,6]
{-}
    print $ flatten [[2,6],[8,1,4],[],[1]]
    print $ remove [1,4,5,3,4,5,1,2,7,4,2] [2,4]
--}
    print $ myLength [1,3..10]
{-
    print $ oddsNevens [1,4,5,3,4,5,1,2,7,4,2]
    print $ primeDivisors 255
--}
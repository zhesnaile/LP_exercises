myLength :: [Int] -> Int

myMaximum :: [Int] -> Int

average :: [Int] -> Float

buildPalindrome :: [Int] -> [Int]

remove :: [Int] -> [Int] -> [Int] 

flatten :: [[Int]] -> [Int]

oddsNevens :: [Int] -> ([Int],[Int])

primeDivisors :: Int -> [Int]

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
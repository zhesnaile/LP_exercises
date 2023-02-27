flatten::[[Int]] -> [Int]
flatten [] = []
flatten (m:n) = foldl (++) m n

myLength::String -> Int
myLength = foldl (\acc _ -> acc +1) 0

myReverse :: [Int] -> [Int]
myReverse = foldl (flip (:)) []

main::IO()
main = do
    print $ flatten [[1,2,3],[4,5],[6],[],[3,3]]
    print $ myLength "Albert"
    print $ myReverse [1..10]
{-    print $ countIn [[3,2,3],[3],[], [2,2]] 3
    print $ firstWord "  Volem pa amb oli  "
-}
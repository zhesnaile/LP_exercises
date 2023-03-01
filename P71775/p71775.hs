countIf :: (Int -> Bool) -> [Int] -> Int
countIf f l = length $ filter f l

pam :: [Int] -> [Int -> Int] -> [[Int]]
pam l fl = [map f l | f <- fl]

pam2 :: [Int] -> [Int->Int] -> [[Int]]
pam2 l fl = [map ($ x) fl | x <- l]

filterFoldl :: (Int -> Bool) -> (Int -> Int -> Int) -> Int -> [Int] -> Int
filterFoldl f op accum l = foldl op accum $ filter f l

insert :: (Int -> Int -> Bool) -> [Int] -> Int -> [Int]
insert relation l x = left ++ x :  right
    where
        left = takeWhile (not . relation x) l
        right = dropWhile (not . relation x) l

insertionSort :: (Int -> Int -> Bool) -> [Int] -> [Int]
insertionSort relation = foldl (insert relation) []

main::IO()
main = do
    print $ countIf (>5) [1..10]
    print $ pam [1,2,3] [(+1),(*2),(^2)]
    print $ pam2 [1,2,3] [(+1),(*2),(^2)]
    print $ filterFoldl even (*) 1 [4,7,2,4,9,3]
    print $ insert (<) [1,4,6,9,12] 8
    print $ insertionSort (>) [4,5,2,3,1,3]
{--}
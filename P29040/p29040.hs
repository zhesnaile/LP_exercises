insert :: [Int] -> Int -> [Int]
insert [] y = [y]
insert ls@(x:xs) y
    | x < y = x : insert xs y
    | otherwise = y : ls

isort :: [Int] -> [Int]
isort [x] = [x]
isort (x:xs) = insert (isort xs) x

remove :: [Int] -> Int -> [Int]
remove (x:xs) y
    | x == y = xs
    | otherwise = x : remove xs y

ssort :: [Int] -> [Int]
ssort [] = []
ssort xs =
    let
        x = minimum xs
    in
        x :  ssort (remove  xs x)

merge :: [Int] -> [Int] -> [Int]
merge [] [] = []
merge [] rl = rl
merge ll [] = ll
merge ll@(x:xs) rl@(y:ys)
    | x < y = x : merge xs rl
    | otherwise = y : merge ll ys


msort :: [Int] -> [Int]
msort [] = []
msort [x] = [x]
msort xs = merge (msort left) (msort right)
    where
        middle :: [Int] -> Int
        middle xs = length xs `div` 2
        (left, right) = splitAt (middle xs) xs


qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs) =
    let
        lesserEqual xs x = [y| y <- xs, y <= x]
        greater xs x = [y | y <- xs, y > x]
    in
        qsort(lesserEqual xs x) ++ x:qsort(greater xs x)


{-
genQsort :: Ord a => [a] -> [a]
--}

main::IO()
main = do
    print $ insert [10,20,30,40] 25
    print $ insert [10,20,30,40] 20
    print $ isort [6,5,2,5,6,8]
    print $ remove [6,4,3,5,2,3] 2
    print $ remove [6,4,3,5,2,3] 6
    print $ ssort [6,5,2,5,6,8]
    print $ merge [1,2,5,7,8] [2,4,7,9]
    print $ msort [6,5,2,5,6,8]
    print $ qsort [6,5,2,5,6,8]
{-    print $ genQsort [5.0,3.0,2.5]
    print $ genQsort ["jordi", "albert", "josep"]
    print $ genQsort "antaviana"
--}
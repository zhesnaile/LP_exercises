eql :: [Int] -> [Int] -> Bool
eql ls rs = ls == rs

prod :: [Int] -> Int
prod = foldl (*) 1

prodOfEvens :: [Int] -> Int
prodOfEvens = prod . filter even

powersOf2::[Int]
powersOf2 = iterate (*2) 1

scalarProduct :: [Float] -> [Float] -> Float
scalarProduct xs ys = sum $ zipWith (*) xs ys

main::IO()
main = do
    print $ eql [1,2,3] [1,2,3]
    print $ eql [1,2,3] [3,2,1]
    print $ eql [1,2,3] [1,2,3,4]
    print $ prod [2,10,5]
    print $ prodOfEvens [2,10,5]
    print $ take 5 powersOf2
    print $ scalarProduct [2.0,1.0,5.0] [3.0,2.0,2.0]
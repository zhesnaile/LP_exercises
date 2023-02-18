absValue :: Int -> Int
absValue n
    | n >= 0    = n
    | otherwise = -n 

power :: Int -> Int -> Int
power _ 0 = 1
power x p
    | even p = y * y
    | otherwise = y * y * x
    where
        y = power x p_halved
        p_halved = div p 2

isPrime :: Int -> Bool
isPrime n
    | n <= 1    = False
    | otherwise = null [x | x <- [2..isqrt n], mod n x == 0]
    where
        isqrt :: Int -> Int
        isqrt x = floor $ sqrt (fromIntegral x :: Float)

slowFib :: Int -> Int
--{-
slowFib n
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = slowFib(n-1) + slowFib(n-2)
---}
{-
slowFib 0 = 0
slowFib 1 = 1
slowFib n = slowFib(n-1) + slowFib(n-2)
---}

data Matrix2x2 = Matrix 
    { x00 :: Int, x01 :: Int
    , x10 :: Int, x11 :: Int
    }

quickFib :: Int -> Int
quickFib n = 0

main::IO()
main = do
    print $ absValue (-666)
    print $ power 2 3
    print $ isPrime 17
    print $ slowFib 5
    print $ quickFib 40
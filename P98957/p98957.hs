import Text.XHtml (base)
ones :: [Integer]
ones = repeat 1

nats :: [Integer]
nats = iterate (+1) 0

ints :: [Integer]
ints = iterate next_value 0
    where
        next_value :: Integer -> Integer
        next_value x
            | x <= 0 = (-x) + 1
            | otherwise = -x

triangulars :: [Integer]
triangulars = triangle 0 
    where
        triangle :: Integer -> [Integer]
        triangle n = n*(n+1) `div` 2 : triangle (n + 1)

factorials :: [Integer]
factorials = scanl (*) 1 $! iterate (+1) 1

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

primes :: [Integer]
primes = filter isPrime nats
    where
        isPrime :: Integer -> Bool
        isPrime n
            | n <= 1 = False
            | otherwise = null [x | x <- [2.. floor $ sqrt $ fromIntegral n], mod n x == 0]

hammings :: [Integer]
hammings = 1 : map (2*) hammings `union` map (3*) hammings `union` map (5*) hammings
    where
        union a@(x:xs) b@(y:ys)
            | x < y  = x : union xs b
            | x == y = x : union xs ys
            | otherwise = y : union a ys

lookNsay :: [Integer]
lookNsay = []

tartaglia :: [[Integer]]
tartaglia = [[]]

main::IO()
main = do
    print $ take 8 ones
    print $ take 8 nats
    print $ take 8 ints
    print $ take 8 triangulars
    print $ take 8 factorials
    print $ take 8 fibs
    print $ take 8 primes
    print $ take 8 hammings
    print $ take 8 lookNsay
    print $ take 6 tartaglia
{---}
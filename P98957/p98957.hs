ones :: [Integer]
ones = repeat 1

nats :: [Integer]
nats = iterate (+1) 0

ints :: [Integer]
ints = 

main::IO()
main = do
    print $ take 8 ones
    print $ take 8 nats
    print $ take 8 ints
{-    print $ take 8 triangulars
    print $ take 8 factorials
    print $ take 8 fibs
    print $ take 8 primes
    print $ take 8 hammings
    print $ take 8 lookNsay
    print $ take 6 tartaglia
--}
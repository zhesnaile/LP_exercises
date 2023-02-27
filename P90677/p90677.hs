myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl _ a [] = a
myFoldl f a (b:bs) = myFoldl f (f a b) bs

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ b [] = b
myFoldr f b (a:as) = f a (myFoldr f b as) 

myIterate :: (a->a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

myUntil :: (a->Bool) -> (a->a) -> a -> a
myUntil con f x = head (filter con $ iterate f x)

myMap :: (a->b) -> [a] -> [b]
myMap f as = [f b | b <- as]

myFilter :: (a->Bool) -> [a] -> [a]
myFilter f as = [b | b <- as, f b]

myAll :: (a -> Bool) -> [a] -> Bool
myAll f as = and (map f as)

myAny :: (a -> Bool) -> [a] -> Bool
myAny f as = or (map f as)

myZip :: [a] -> [b] -> [(a, b)]
myZip _ [] = []
myZip [] _ = []
myZip (a:as) (b:bs) = (a, b) : myZip as bs

myZipWith :: (a->b->c) -> [a] -> [b] -> [c]
myZipWith f as bs = [f a b | (a,b) <- zip as bs]

main::IO()
main = do
    print $ myFoldl (+) 1 [1..5]
    print $ myFoldr (+) 1 [1..5]
    print $ take 10 $ myIterate (*2) 1
    print $ myUntil (>100) (*2) 1
    print $ myMap ("la "++) ["joana", "mireia"]
    print $ myFilter odd [1..10]
    print $ myAll odd [1,3,5,3,1]
    print $ myAny odd [2,4,6,8,10]
    print $ myZip [1..4] [1..3]
    print $ myZipWith (+) [1..4] [1..3]
{--}
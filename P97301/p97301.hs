fizzBuzz :: [Either Int String]
fizzBuzz = map fizz [0..]
    where
        fizz :: Int -> Either Int String
        fizz n
            | n `mod` 15 == 0 = Right "FizzBuzz"
            | n `mod` 3 == 0  = Right "Fizz"
            | n `mod` 5 == 0 = Right "Buzz"
            | otherwise = Left n

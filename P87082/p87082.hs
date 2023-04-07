bmiDiagnose :: Float -> Float -> String
bmiDiagnose mass height
    | bmi < 18 = "underweight"
    | bmi < 25 = "normal weight"
    | bmi < 30 = "overweight"
    | bmi < 40 = "obese"
    | otherwise = "severely obese"
    where
        bmi = mass / (height * height)

getWord = do
    c <- getChar
    if c `elem` ['\n', ' ']
        then return ""
        else do
            w <- getWord
            return (c:w)

main :: IO()
main = do 
    name <- getWord
    if name == "*" 
        then return ()
        else do
            w <- getWord
            h <- getWord
            let reply = name ++ ": " ++ bmiDiagnose (read w :: Float) (read h :: Float)
            putStrLn reply
            main
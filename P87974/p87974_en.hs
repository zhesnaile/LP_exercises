name2reply :: String -> Maybe String
name2reply [] = Nothing
name2reply x 
  | head x `elem` ['a', 'A'] = Just "Hello!"
  | otherwise = Just "Bye!"

main :: IO ()
main = do
  x <- getLine
  let y = name2reply x
  case y of
    Just something -> putStrLn something
    _ -> return ()
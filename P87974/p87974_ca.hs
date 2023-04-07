name2reply :: String -> Maybe String
name2reply [] = Nothing
name2reply x 
  | last x `elem`['a', 'A'] = Just "Hola maca!"
  | otherwise = Just "Hola maco!"

main :: IO ()
main = do
  x <- getLine
  let y = name2reply x
  case y of
    Just something -> putStrLn something
    _ -> return ()
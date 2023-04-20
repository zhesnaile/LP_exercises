type Peg = String

hanoiM :: Integer -> Peg -> Peg -> Peg -> IO ()
hanoiM 0 _ _ _ = return ()
hanoiM n a b c = do
  hanoiM (n-1) a c b
  putStrLn $ a ++ " -> " ++ b
  hanoiM (n-1) c b a

main :: IO()
main = do
  x <- getLine
  let inputs = words x
  let n = read (head inputs) :: Integer
  let [_, a, b, c] = inputs
  hanoiM n a b c

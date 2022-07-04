intercal :: [a] -> [a] -> [a]
intercal [] ys = ys
intercal xs [] = xs
intercal (x : xs) (y : ys) = [x] ++ [y] ++ intercal xs ys

main :: IO ()
main = do
  a <- readLn :: IO [Int]
  b <- readLn :: IO [Int]
  print $ intercal a b
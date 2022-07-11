primo :: Integral a => a -> Bool
primo n = foldl (\p x -> if n `mod` x == 0 then False else p) True [2..n-1]

main :: IO ()
main = do
  a <- readLn :: IO Int
  print $ primo a
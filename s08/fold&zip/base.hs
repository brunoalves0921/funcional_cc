base :: Int -> Int -> [Char]
base n b
  | n <= 0 = []
  | otherwise = base num b ++ [res]
  where
    texto = ['0'..'9']++['A'..'Z']
    resto = n `mod` b
    num = div (n - resto) b
    res = texto!!resto

main :: IO ()
main = do
    a <- readLn :: IO Int
    b <- readLn :: IO Int
    print $ base a b

main' :: IO ()
main' = do
  print $ base 25 10 == "25"
  print $ base 17 2 == "10001"
  print $ base 26 16 == "1A"
  print $ base 26012 36 == "K2K"
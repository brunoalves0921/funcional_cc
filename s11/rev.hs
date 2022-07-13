rev n = fn n 0
  where
    fn n y
        | n > 0 = fn (n `quot` 10) ((10*y) + (n `rem` 10))
        | otherwise = y

main = do
    print $ rev 1923
    print $ rev 123
    print $ rev 39402
    print $ rev 5
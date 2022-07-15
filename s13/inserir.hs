inserir y (x:xs)
    | y < x = y : x : xs
    | y > last xs = x:xs ++ [y]
    | y > x && y < (head xs) = x : y : xs
    | otherwise = x : inserir y xs

main = do
    print $ inserir 3 [2,7,12] -- [2,3,7,12]
    print $ inserir 1 [2,7,12] -- [1,2,7,12]
    print $ inserir 10 [2,7,12] -- [2,7,10,12]
    print $ inserir 15 [2,7,12] -- [2,7,12,15]
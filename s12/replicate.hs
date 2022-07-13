myreplicate n s
    | n == 0 = []
    | otherwise = s : myreplicate (n-1) s

main = do
    print $ myreplicate 4 0 -- [0,0,0,0]
    print $ myreplicate 2 True -- [True,True]
    print $ myreplicate 3 [5] --([[5,5],[5,5,5]] != ["banana", "banana", "banana"]) kkkkkk
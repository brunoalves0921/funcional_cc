vetFib x = take x $ map fibonacci [0..]

fibonacci x
    | x == 0 = 0
    | x == 1 = 1
    | otherwise = fibonacci (x-1) + fibonacci (x-2)

main = do
    print $ vetFib 2 == [0,1]
    print $ vetFib 6 == [0,1,1,2,3,5]
    print $ vetFib 9 == [0,1,1,2,3,5,8,13,21]
myMerge xs ys = ordenar $ juntar xs ys     

juntar [] xy = xy
juntar xy [] = xy
juntar (x:xs) (y:ys) = x : juntar xs (y:ys)  

ordenar [] = []
ordenar [x] = [x]
ordenar (x:xs) = ordenar [y | y <- xs, y <= x] ++ [x] ++ ordenar [y | y <- xs, y > x]

main = do
    print $ myMerge [1,3] [7,7,9] -- [1,3,7,7,9]
    print $ myMerge [7,7,9] [1,3] -- [1,3,7,7,9]
    print $ myMerge [1,3,5] [4,4,6,7] -- [1,3,4,4,5,6,7]
    print $ myMerge [4,4,5,6,7] [1,3] -- [1,3,4,4,5,6,7]
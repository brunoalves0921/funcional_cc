intersec list1 list2 
    | list1 == [] = []
    | list2 == [] = []
    | otherwise = [x | x <- list1, elem x list2]
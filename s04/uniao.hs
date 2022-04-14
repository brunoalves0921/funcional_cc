unir list1 list2 
    | list1 == [] = list2
    | list2 == [] = list1
    | otherwise = (head list1) : (unir (tail list1) list2)

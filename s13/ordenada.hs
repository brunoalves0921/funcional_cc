ordenada [] = True
ordenada [x] = True
ordenada (x:y:xs) = x <= y && ordenada (y:xs) 

main = do
    print $ ordenada [1,2,3,5] == True
    print $ ordenada [1,0,2,5] == False
    print $ ordenada [1,2,3,2] == False
    print $ ordenada [1,2,2,2,5] == True
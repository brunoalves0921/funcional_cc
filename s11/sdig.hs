sdig n 
    | n == 0 = 0
    | otherwise = n `mod` 10 + sdig (n `div` 10)
main = do    
    print $ sdig 4132 -- == 10
    print $ sdig 328464584658 -- == 63
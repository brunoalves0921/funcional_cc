expoentes x y = (expoentes' x y)-1

expoentes' x y = length $ takeWhile (>=1) $ iterate func x
    where func x = if(x `mod` y == 0) then div x y else 0

main = do
    print $ expoentes 7 2
    print $ expoentes 4 2
    print $ expoentes 8 2
    print $ expoentes 24 2
    print $ expoentes 1024 2
    print $ expoentes 150 5
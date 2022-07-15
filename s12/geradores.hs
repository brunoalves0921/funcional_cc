import Data.List (unfoldr, transpose)

gerador1Rec = gerador 0
    where gerador x
            | x == 0 = [0] ++ gerador 1
            | otherwise = (x) : (-x) : gerador (x+1)

gerador2Rec = gerador 1
    where gerador x
            | mod x 2 == 0 = (-x) : gerador (x+1) 
            | otherwise = (x) : gerador (x+1)

gerador3Rec = gerador 1
    where gerador x
            |x == 1 = (x) : gerador (x+1) 
            |otherwise = (2*x) : gerador (x*2)

gerador4Rec x = gerador x
    where gerador x
            | x <= 0 = [] 
            |otherwise = (x) : gerador (div x 2) 

-----------------------------------------------
gerador1Iterate = iterate  func 0
    where func x
            | x == 0 = 1
            | x > 0 = -x
            | otherwise = (-x)+1 

gerador2Iterate = iterate  func 1
    where func x = if mod x 2 == 1 then (-(x+1)) else (-(x-1))

gerador3Iterate = iterate (*2) 1

gerador4Iterate x = iterate (/2) x

---------------------------------------------
gerador1Unfoldr = take 10 $ unfoldr func 0
    where func x = if x == 0 then Just(0,1) else Just((-x),(x+1))

gerador2Unfold = unfoldr func 1
    where func n = if mod n 2 == 0 then Just (n, (-n)+1) else Just (n, -(n+1))

gerador3Unfold = unfoldr func 1
    where func n = if n < 0 then Nothing else Just (n, n*2)

gerador4Unfold x = unfoldr func x
    where func n = if n <= 0 then Nothing else Just (n, div n 2)
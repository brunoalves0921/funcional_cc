at' xs 0 = head xs
at' (x:xs) n = at xs (n-1)

at xs n = at' xs n2
    where
        n2 = mod n $ length xs
        at' xs 0 = head xs
        at' (x:xs) n = at' xs (n -1)

atloop xs n = at xs n2
    where
        n2 = (mod n $ length xs) -1 
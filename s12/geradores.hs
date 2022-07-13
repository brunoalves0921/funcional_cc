a1 = gerador 0
    where gerador n 
            | n > 0 = [n] ++ gerador (n * (-1))
            | otherwise = [n] ++ gerador (n * (-1) + 1)
qtsImpares xs = foldl isImpar 0 xs
    where isImpar cont x = if x `mod` 2 /= 0 then cont + 1 else cont
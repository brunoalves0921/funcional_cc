indice n (x:xs) = if n == 0 then x else indice (n - 1) xs

elemento n xs = indice n' xs
        where len = length xs 
              n' = if n < 0 then n + len else n
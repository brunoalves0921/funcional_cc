contar x xs = foldl quantos 0 xs 
    where quantos acc y = if x == y then acc + 1 else acc
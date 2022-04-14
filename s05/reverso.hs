reverso lista 
    | lista == [] = []
    | otherwise = (last lista) : reverso (init lista)
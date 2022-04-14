splitints list = ([x | x<-list, odd x], [x | x<-list, even x])
separar [] = ([],[])
separar (x:xs) = (x:fst(separar xs), snd(separar xs))
    where (impares, pares) = separar xs
fn x tup = ([x | odd x] ++ (fst tup), [x | even x] ++ (snd tup))
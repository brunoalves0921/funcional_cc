deletee :: Eq a => a -> [a] -> [a]
deletee n [] = []
deletee n (x : xs) = if x /= n then x : deletee n xs else xs
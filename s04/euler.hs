euler n = sum [x | x <- [1..n-1], isPrime x 3 || isPrime x 5]
    where isPrime x y = mod x y == 0
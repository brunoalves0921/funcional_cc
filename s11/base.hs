base a b = fn a []
    where
        fn x xs
            | x > 0 = fn (x `quot` b) (text!!(x `rem` b) : xs)
            | otherwise = xs
            where text = (['0'..'9'] ++ ['a'..'z'])

main = do
    print $ base 25 10
    print $ base 17 2
    print $ base 26 16
    print $ base 26012 36
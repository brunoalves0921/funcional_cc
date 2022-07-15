decompor x 
    | x  > 9 = decompor (x `div` 10) ++ [x `mod` 10]
    | otherwise = [x]

decomporIterate x = reverse $ take (length y) $ (`mod`10) <$> (iterate (\x -> x `div` 10) x)
    where y = show x 

main = do
    print $ decompor 0
    print $ decomporIterate 0
    print $ decompor 123
    print $ decomporIterate 123
    print $ decompor 549050
    print $ decomporIterate 549050
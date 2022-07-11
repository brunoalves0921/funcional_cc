myelemIndex :: (Num a, Enum a) => Int -> [Int] -> Maybe a
myelemIndex n xs 
  | length find > 0 = Just $ snd $ head find
  | otherwise = Nothing
  where
    find = filter (\(x,_) -> x == n) $ zip xs [0..]

main :: IO ()
main = do
  a <- readLn :: IO Int
  b <- readLn :: IO [Int]
  print $ myelemIndex a b

main' :: IO ()
main' = do
  print $ myelemIndex 4 [1,2,6,4,3] == Just 3
  print $ myelemIndex 5 [1,2,6,4,3] == Nothing
  print $ myelemIndex 4 [] == Nothing
  print $ myelemIndex 1 [1,2,6,4,3] == Just 0
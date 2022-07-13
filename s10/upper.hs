import Data.Maybe

upper s = map toUpper s
  where
    toUpper s
        | elem s ['a'..'z'] = fromJust $ lookup s $ zip ['a'..'z'] ['A'..'Z']
        | otherwise = s

main = do
    print $ upper "abc"
    print $ upper "ola mundo"
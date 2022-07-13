import Data.Char

titulo s = format ' ' s
  where
    format ' ' (x:xs) = toUpper x : format x xs
    format _ (x:xs) = toLower x : format x xs
    format ' ' [] = []
    format _ [] = []
    
main = do
    print $ titulo "XiTaOziNho e ChoroRo"
    print $ titulo "OLA, MUNDO!"
    print $ titulo "abc"

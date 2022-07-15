text = ['0'..'9'] ++ ['A'..'Z']
pegarValor x = (text !! x)

base 0 b = []
base n b = base (n `div` b) b ++ [pegarValor (n `mod` b)]

main = do
    print $ base 25 10
    print $ base 17 2
    print $ base 26 16
    print $ base 26012 36
pegarIguais xs = takeWhile (== head xs) xs

compac xs 
    |xs == [] = []
    |length (pegarIguais xs) == 1 = [head (pegarIguais xs)] : compac (drop (length (pegarIguais xs)) xs)
    |otherwise = ( [head (pegarIguais xs)] ++ [length (pegarIguais xs)] ) : compac (drop (length (pegarIguais xs)) xs)
    
main = do
    print $ compac [] -- []
    print $ compac [1,1,1] -- [[1,3]]
    print $ compac [1,2,3] -- [[1],[2],[3]]
    print $ compac [2,2,2,3,4,4,2,9,5,2,4,5,5,5] -- [[2,3],[3],[4,2],[2],[9],[5],[2],[4],[5,3]]
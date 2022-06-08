import Data.List
import Data.Maybe
------------------------------------------EXISTS------------------------------------------

exists y xs = 
    if xs == []
        then False
        else if xs !! 0 == y
            then True
            else exists y (tail xs)
-----------------------------------------NEIGHBORH-----------------------------------------
neighborhood xs index limit = drop (index - limit) . take (index + limit + 1) $ xs
-----------------------------------------DIG2CHAR------------------------------------------
dig2Char dig = fromJust (lookup dig (zip [0..9] ['0'..'9']))
--------------------------------------------SET--------------------------------------------
set xs index value = list1 ++ (valueChar : list2)
    where (list1,_:list2) = splitAt index xs
          valueChar = dig2Char value
------------------------------------------FITTEST-------------------------------------------
fit (xs, lim) index value = not (exists valueChar sub)
    where 
        sub = neighborhood xs index lim
        valueChar = dig2Char value
-----------------------------------------GETHOLETEST----------------------------------------
getHoles xs = elemIndices '.' xs
------------------------------------------SOLVE---------------------------------------------
solve (xs, lim) holes hindex
    | sizeHolesPath == hindex = Just xs
    | length filteredResult == 0 = Nothing
    | otherwise = if length filteredResult == 0
        then Nothing
        else head filteredResult
    where
        sizeHolesPath = length holes
        possibleValues = zip [0..] (foldl (\acc x -> acc ++ [fit (xs, lim) (holes!!hindex) x]) [] [0..lim])
        result = [fst x | x <- possibleValues, snd x]
        variable = [solve (set xs (holes!!hindex) r, lim) holes (hindex+1) | r <- result]
        filteredResult = [x | x <- variable, isJust x]

----------------------------------------MAIN-----------------------------------------------
mainSolver xs lim = fromJust $ solve (xs, lim) (getHoles xs) 0


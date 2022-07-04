myreplicate :: (Num t1, Ord t1) => t1 -> t2 -> [t2]
myreplicate 1 y = [y]
myreplicate x y
  | x > 0 = y : myreplicate (x -1) y
  | otherwise = []
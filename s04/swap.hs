swap' list p q = left ++ [elemQ] ++ middle ++ [elemP] ++ right
    where elemP = list !! p 
          elemQ = list !! q
          left = take p list
          middle = take (q - p - 1) (drop (p + 1) list)
          right = drop (q + 1) list
swap list p q = if (p <= q) then swap' list p q else swap' list q p
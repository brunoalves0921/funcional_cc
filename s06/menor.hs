menor xs = foldl isMenor 0 xs
    where isMenor acc x = if acc == 0 then x else if x < acc then x else acc
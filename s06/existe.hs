import Control.Monad (when)
existe x xs = foldl fn False xs
    where fn bool x = bool || x == xs
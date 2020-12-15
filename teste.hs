raizInt :: Int -> Int
raizInt n
    | n < 0 = error "negative number"
    | n == 0 = 0
    | otherwise = length (takeWhile (<= n) (map quad [1..]))
    where
        quad x = x * x
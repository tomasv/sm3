
module Extra where


-- generate interval points, starting from a to b, split into n intervals
intervals a b n = let h = (b - a) / n
                   in [a, (a + h)..b]

-- inclusive takeWhile version
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs) | p x       = x : takeWhile' p xs
                    | otherwise = [x]

-- split into parts of size n
splitInto :: Int -> [a] -> [[a]]
splitInto n [] = []
splitInto n xs  = left : splitInto n right
    where (left, right) = splitAt n xs

-- Remove element at index i from list
omitElementAt :: Int -> [a] -> [a]
omitElementAt i list = let (left, right) = splitAt i list in left ++ (tail right)

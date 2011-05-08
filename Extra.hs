
module Extra where

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

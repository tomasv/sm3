
module Extra where

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs) | p x       = x : takeWhile' p xs
                    | otherwise = [x]

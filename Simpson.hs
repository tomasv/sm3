module Simpson where

myFunc x = ((sqrt x) - 1) ^ 2
myFunc2 = (^2) . (subtract 1) . sqrt

simpsonA :: (Double -> Double) -> [Double] -> Double
simpsonA f xs = ((b - a) / (3*n)) * ((f a) + 4 * oddSum + 2 * evenSum + (f b))
    where oddSum = sum (map (f . (xs !!)) [1,3..size-1])
          evenSum = sum (map (f . (xs !!)) [2,4..size-2])
          b = last xs
          a = head xs
          size = pred . length $ xs
          n = (fromIntegral size) :: Double

simpsonB :: (Double -> Double) -> [Double] -> Double
simpsonB f xs = (h / 3) * sum (map g [0,2..(n - 2)])
    where g i = f (xs !! i) + 4 * f ((xs !! (i + 1))) + f (xs !! (i + 2))
          n = pred . length $ xs
          h = ((last xs) - (head xs)) / (fromIntegral n)

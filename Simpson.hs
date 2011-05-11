module Simpson where

import Datatypes
import Extra
import Debug.Trace

myFunc x = ((sqrt x) - 1) ^ 2
myFunc2 = (^2) . (subtract 1) . sqrt

runSeriesP err f a b n = takeWhile' (\(DResult _ e _) -> e >= err) $ series firstResult f a b (n * 2)
    where firstResult = DResult current e n
          current = simpsonA f (intervals a b n)
          previous = simpsonA f (intervals a b (n/2))
          e = abs (current - previous) / (2^2 - 1)

series oldResult f a b n = oldResult : series newResult f a b (n * 2)
    where (DResult oldApproximation _ _) = oldResult
          err = abs (newApproximation - oldApproximation) / (2^2 - 1)
          newApproximation = simpsonA f (intervals a b n)
          newResult = (DResult newApproximation err n)


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

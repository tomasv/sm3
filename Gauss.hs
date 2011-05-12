module Gauss where

import Datatypes
import Extra

-- 3 point Gauss coeficients and nodes
c1 = 0.5555556
c2 = 0.8888889
c3 = 0.5555556

x1 = 0.774596669
x2 = 0.0
x3 = -0.774596669

normalize a b x = let alfa = (b - a) / 2
                      beta = (a + b) / 2
                   in alfa * x + beta

gauss3 f a b = ((b - a) / 2) * (c1 * (g x1) + c2 * (g x2) + c3 * (g x3))
    where g = f . (normalize a b)

intervalPairs a b n = let shifted = drop 1 $ cycle (intervals a b n)
                          regular = init $ intervals a b n
                       in regular `zip` shifted

complexGauss3 a b f n = sum $ map (\(x, y) -> gauss3 f x y) (intervalPairs a b n)

runSeriesP err f a b n = takeWhile' (\(DResult _ e _ _) -> e >= err) (runSeries a b f n)

runSeries a b f n = series (DResult result err n 0.0) a b f (n * 2)
    where result = complexGauss3 a b f n
          previous = complexGauss3 a b f (n / 2)
          err = abs (result - previous) / (2^6 - 1)

series result a b f n = result : series newResult a b f (n * 2)
    where approximation = complexGauss3 a b f n
          (DResult oldApproximation oldErr _ _) = result
          err = abs (approximation - oldApproximation) / (2^6 - 1)
          newResult = (DResult approximation err n (oldErr / err))

gaussError a b f n = ((sn n) - (sn (n / 2))) / (2^6 - 1)
    where sn m = complexGauss3 a b f m

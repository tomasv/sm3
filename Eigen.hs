module Eigen where

import Control.Monad.Writer
import Vector
import Array (listArray, bounds, elems)
import Matrix.LU (inverse)

-- runLambdaSeries :: [[Double]] -> Double -> Double -> [Double]
-- runLambdaSeries a l r = let lambda = lambdaApproximation l r
--              in lambdaSeries lambda a (take (length a) $ repeat lambda)

lambdaSeriesWithPrecision :: Double -> Double -> [[Double]] -> [Double] -> [Double]
lambdaSeriesWithPrecision err lambda a x = if precisionCheck2 && precisionCheck1
                                              then [lambda, lambda']
                                              else lambda : lambdaSeriesWithPrecision err lambda' a x'
    where (lambda', x') = nextLambdaAndVector lambda a x
          precisionCheck1 = vectorSpectralRadius (x' |-| x) <= err
          precisionCheck2 = abs (lambda' - lambda) <= err

lambdaSeries :: Int -> Double -> [[Double]] -> [Double] -> [Double]
lambdaSeries 0 lambda a x = []
lambdaSeries n lambda a x =
    let (lambda', x') = nextLambdaAndVector lambda a x
    in lambda : (lambdaSeries (n - 1) lambda' a x')

lambdaSeriesW :: Int -> Double -> [[Double]] -> [Double] -> Writer [[Double]] [Double]
lambdaSeriesW 0 lambda a x = return []
lambdaSeriesW n lambda a x = do
    let (lambda', x') = nextLambdaAndVector lambda a x
    l <- lambdaSeriesW (n - 1) lambda' a x'
    tell [x]
    return $ lambda : l

nextLambdaAndVector :: Double -> [[Double]] -> [Double] -> (Double, [Double])
nextLambdaAndVector lambda a x = ((a ||*| x') |.| x', x')
    where x' = map (/ (vectorSpectralRadiusSquared y')) y'
          y' = ae ||*| x
          ae = inverseMatrix $ a ||-|| (lambda *|| e)
          e = identityMatrix (length a)

identityMatrix :: Int -> [[Double]]
identityMatrix n = let row = 1.0 : (take (n - 1) $ repeat 0.0)
                       nextRow r = take n $ drop (n - 1) $ cycle r
                   in scanl (\x y -> nextRow x) row [1..n-1]

lambdaApproximation :: Double -> Double -> Double
lambdaApproximation a b = (a + b) / 2

inverseMatrix :: [[Double]] -> [[Double]]
inverseMatrix m = splitInto width $ elems a
    where height = length m
          width  = length $ head m
          a = inverse $ listArray ((1::Int,1::Int), (width, height)) (concat m)

splitInto :: Int -> [a] -> [[a]]
splitInto n [] = []
splitInto n xs  = left : splitInto n right
    where (left, right) = splitAt n xs

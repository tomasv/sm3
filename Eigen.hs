module Eigen where

import Vector

runLambdaSeries :: [[Double]] -> Double -> Double -> [Double]
runLambdaSeries a l r = let lambda = lambdaApproximation l r
             in lambdaSeries lambda a lambda

lambdaSeries :: [[Double]] -> Double -> [Double] -> [Double]
lambdaSeries lambda a x = lambda : nextLambda lambda a x

nextLambda :: Double -> [[Double]] -> [Double] -> Double
nextLambda lambda a x = (a ||*| x') |.| x'
    where x' = y' / (spectralRadius y')
          y' = ae ||*| x
          ae = inverse $ a ||-|| (lambda *|| e)

lambdaApproximation :: Double -> Double -> Double
lambdaApproximation a b = (a + b) / 2

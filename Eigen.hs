module Eigen where

import MatrixVector
import Datatypes
import Extra

import Debug.Trace

approximationsUntilPrecise :: Double -> Double -> Matrix -> Vector -> [Result]
approximationsUntilPrecise err lambda a x = takeWhile' (check err) (series lambda a x)
    where check err (EigenResult _ _ err1 err2) = err1 >= err || err2 >= err

series :: Double -> Matrix -> Vector -> [Result]
series lambda a x = result : series lambda' a x'
    where result@(EigenResult lambda' x' _ _) = nextValueAndVector lambda a x

nextValueAndVector :: Double -> Matrix -> Vector -> Result
nextValueAndVector lambda a x = EigenResult lambda' x' err1 err2
    where lambda' = (a ||*| x') |.| x'
          x' = correctedVector x y'
          y' = ae ||*| x
          ae = inverseMatrix $ a ||-|| (lambda *|| e)
          e = identityMatrix (length a)
          err1 = vectorSpectralRadiusSquared (x' |-| x)
          err2 = abs (lambda' - lambda)

correctedVector :: Vector -> Vector -> Vector
correctedVector x y' = vectorCorrection err' x'
    where x' = map (/ (vectorSpectralRadiusSquared y')) y'
          err' = vectorSpectralRadiusSquared (x' |-| x)

p :: Double
p = 0.01

vectorCorrection :: Double -> Vector -> Vector
vectorCorrection err x' | (err - p) < 2 && (err + p) > 2 = map (negate) x'
                        | otherwise = x'

lambdaApproximation :: Double -> Double -> Double
lambdaApproximation a b = (a + b) / 2

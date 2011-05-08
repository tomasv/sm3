module MatrixVector where

import Array (listArray, bounds, elems)
import Matrix.LU (inverse)

import Extra

type Matrix = [[Double]]
type Vector = [Double]

-- Spectral radius of a matrix
spectralRadius :: Matrix -> Double
spectralRadius m = maximum (map (abs) (concat m))

-- Spectral radius of a vector
vectorSpectralRadius :: Vector -> Double
vectorSpectralRadius v = maximum (map (abs) v)

-- Spectral radius of a vector square
vectorSpectralRadiusSquared :: Vector -> Double
vectorSpectralRadiusSquared v = sqrt $ sum $ map (^2) v

-- Generate an identity matrix with size n
identityMatrix :: Int -> Matrix
identityMatrix n = let row = 1.0 : (take (n - 1) $ repeat 0.0)
                       nextRow r = take n $ drop (n - 1) $ cycle r
                   in scanl (\x y -> nextRow x) row [1..n-1]

-- Get an inverse matrix
inverseMatrix :: Matrix -> Matrix
inverseMatrix m = splitInto width $ elems a
    where height = length m
          width  = length $ head m
          a = inverse $ listArray ((1::Int,1::Int), (width, height)) (concat m)

-- Vector subtraction
(|-|) :: Vector -> Vector -> Vector
(|-|) a b = zipWith (-) a b

-- Vector addition
(|+|) :: Vector -> Vector -> Vector
(|+|) a b = zipWith (+) a b

-- Matrix vector multiplication
(||*|) :: Matrix -> Vector -> Vector
(||*|) a b = zipWith (vectorMult) a (repeat b)
    where vectorMult x y = sum $ zipWith (*) x y

(||-||) :: Matrix -> Matrix -> Matrix
(||-||) a b = zipWith (zipWith (-)) a b

-- Vector dot product
(|.|) :: Vector -> Vector -> Double
(|.|) a b = sum $ zipWith (*) a b

-- Constant and vector multiplication
(*|) :: Double -> Vector -> Vector
(*|) a b = map (* a) b

-- Constant and matrix multiplication
(*||) :: Double -> Matrix -> Matrix
(*||) a b = map (map (* a)) b



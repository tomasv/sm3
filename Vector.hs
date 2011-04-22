module Vector where

-- Spectral radius of a matrix
spectralRadius :: [[Double]] -> Double
spectralRadius m = maximum (map (abs) (concat m))

-- Spectral radius of a vector
vectorSpectralRadius :: [Double] -> Double
vectorSpectralRadius v = maximum (map (abs) v)

-- Vector subtraction
(|-|) :: [Double] -> [Double] -> [Double]
(|-|) a b = zipWith (-) a b

-- Vector addition
(|+|) :: [Double] -> [Double] -> [Double]
(|+|) a b = zipWith (+) a b

-- Matrix vector multiplication
(||*|) :: [[Double]] -> [Double] -> [Double]
(||*|) a b = zipWith (vectorMult) a (repeat b)
    where vectorMult x y = sum $ zipWith (*) x y

-- Vector dot product
(|.|) :: [Double] -> [Double] -> Double
(|.|) a b = sum $ zipWith (*) a b

-- Constant and vector multiplication
(*|) :: Double -> [Double] -> [Double]
(*|) a b = map (* a) b


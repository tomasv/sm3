module Jacobi (jacobiApproximationsUntilPrecise, jacobiIterate, jacobiIteration) where

import Vector

jacobiProductSum :: Int -> [Double] -> [Double] -> Double
jacobiProductSum i a x = sum $ zipWith (*) aFiltered xFiltered
    where
        aFiltered = omitElementAt i a
        xFiltered = omitElementAt i x 

jacobiXi :: Int -> [[Double]] -> [Double] -> [Double] -> Double
jacobiXi i a b x = (bi - sigma) / aii
    where
        bi    = b !! i
        ai    = a !! i
        aii   = ai !! i
        sigma = jacobiProductSum i ai x

jacobiIteration :: [[Double]] -> [Double] -> [Double] -> [Double]
jacobiIteration a b x = [ jacobiXi i a b x | i <- take (length x) [0..] ]

jacobiIterate :: Int -> [[Double]] -> [Double] -> [Double] -> [Double]
jacobiIterate n a b x = iterate (j) x !! n
    where j = jacobiIteration a b

jacobiApproximationsUntilPrecise :: Double -> [[Double]] -> [Double] -> [Double] -> [[Double]]
jacobiApproximationsUntilPrecise err a b x
    | firstCondition && secondCondition = [x']
    | otherwise = [x'] ++ jacobiApproximationsUntilPrecise err a b x'
    where x' = jacobiIteration a b x
          firstCondition = vectorSpectralRadius ((a ||*| x') |-| b) < err
          secondCondition = vectorSpectralRadius (x' |-| x) < err

-- Other helper functions
omitElementAt :: Int -> [a] -> [a]
omitElementAt i list = let (left, right) = splitAt i list in left ++ (tail right)

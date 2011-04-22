module ConjugateGradient where

import Vector

cgApproximationsUntilPrecise :: Double -> [[Double]] -> [Double] -> [Double] -> [[Double]]
cgApproximationsUntilPrecise err a f x = cgSeries err a f x z p
    where z = initialZ a f x
          p = initialP a f x

cgSeries :: Double -> [[Double]] -> [Double] -> [Double] -> [Double] -> [Double] -> [[Double]]
cgSeries err a f x z p = x' : if precisionCheck err z'
                                 then []
                                 else cgSeries err a f x' z' p'
    where
        r = a ||*| p
        x' = cgIteration x z p r
        z' = z |-| ((tau z r p) *| r)
        p' = z' |+| ((beta z' z) *| p)

cgIteration :: [Double] -> [Double] -> [Double] -> [Double] -> [Double]
cgIteration x z p r = x |-| ((tau z r p) *| p)

precisionCheck :: Double -> [Double] -> Bool
precisionCheck err z = z |.| z < err**2

initialP :: [[Double]] -> [Double] -> [Double] -> [Double]
initialP a f x = (a ||*| x) |-| f

initialZ :: [[Double]] -> [Double] -> [Double] -> [Double]
initialZ = initialP

tau :: [Double] -> [Double] -> [Double] -> Double
tau z r p = (z |.| p) / (r |.| p)

beta :: [Double] -> [Double] -> Double
beta zk1 zk = (zk1 |.| zk1) / (zk |.| zk)

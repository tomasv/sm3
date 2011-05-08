module ConjugateGradient where

import MatrixVector
import Datatypes
import qualified Data.List as L
import Extra

approximationsUntilPrecise :: Double -> Matrix -> Vector -> Vector -> Either String Results
approximationsUntilPrecise err a f x = do
    if convergenceCondition a
       then Right $ seriesPInit err a f x
       else Left "Invalid matrix."

convergenceCondition :: Matrix -> Bool
convergenceCondition m = (L.transpose m) == m

seriesPInit :: Double -> Matrix -> Vector -> Vector -> Results
seriesPInit err a f x = seriesP err a f x z p
    where z = initialZ a f x
          p = initialP a f x

seriesP :: Double -> Matrix -> Vector -> Vector -> Vector -> Vector -> Results
seriesP err a f x z p = takeWhile' (\(CgResult _ e r) -> notPrecise e r) (series a f x z p)
    where notPrecise e r = e >= err

series :: Matrix -> Vector -> Vector -> Vector -> Vector -> Results
series a f x z p = result : series a f x' z' p'
    where
        r = a ||*| p
        result = iteration a x z p r
        (CgResult x' _ z') = result
        p' = z' |+| ((beta z' z) *| p)

iteration :: Matrix -> Vector -> Vector -> Vector -> Vector -> Result
iteration a x z p r = CgResult (x |-| ((tau z r p) *| p)) err res
    where r = a ||*| p
          res = residual z r p
          err = sqrt (res |.| res)

residual :: Vector -> Vector -> Vector -> Vector
residual z r p = z |-| ((tau z r p) *| r)

precisionCheck :: Double -> Vector -> Bool
precisionCheck err z = z |.| z < err**2

initialP :: Matrix -> Vector -> Vector -> Vector
initialP a f x = (a ||*| x) |-| f

initialZ :: Matrix -> Vector -> Vector -> Vector
initialZ = initialP

tau :: Vector -> Vector -> Vector -> Double
tau z r p = (z |.| p) / (r |.| p)

beta :: Vector -> Vector -> Double
beta zk1 zk = (zk1 |.| zk1) / (zk |.| zk)

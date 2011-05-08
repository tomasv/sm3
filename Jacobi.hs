module Jacobi where

import MatrixVector
import Datatypes
import Extra

approximationsUntilPrecise :: Double -> Matrix -> Vector -> Vector -> Either String Results
approximationsUntilPrecise err a b x = do
    if convergenceCondition a
       then Right $ seriesP err 0.0 0.0 a b x
       else Left "Invalid matrix."

convergenceCondition :: Matrix -> Bool
convergenceCondition m = all (rowCheck) (m `zip` [0..])
    where rowCheck (r, index) = (r !! index) > sum (omitElementAt index r)

seriesP :: Double -> Double -> Double -> Matrix -> Vector -> Vector -> Results
seriesP err oldErr oldRes a b x = takeWhile' (\(JacobiResult _ e r) -> notPrecise e r) (iterateJacobi a b x)
    where notPrecise a b = a >= err || b >= err

iterateJacobi :: Matrix -> Vector -> Vector -> Results
iterateJacobi a b x = drop 1 $ iterate (j) (JacobiResult x 0.0 0.0)
    where j = (\(JacobiResult result _ _) -> iteration a b result)

iteration :: Matrix -> Vector -> Vector -> Result
iteration a b x = JacobiResult x' err res
    where x' = [ vectorElement i a b x | i <- [0..(length x) - 1] ]
          err = vectorSpectralRadius (x' |-| x)
          res = vectorSpectralRadius ((a ||*| x') |-| b)

vectorElement :: Int -> Matrix -> Vector -> Vector -> Double
vectorElement i a b x = (bi - sigma') / aii
    where
        bi    = b !! i
        ai    = a !! i
        aii   = ai !! i
        sigma' = sigma i ai x

sigma :: Int -> Vector -> Vector -> Double
sigma i a x = sum $ zipWith (*) aFiltered xFiltered
    where
        aFiltered = omitElementAt i a
        xFiltered = omitElementAt i x 

-- Other helper functions
omitElementAt :: Int -> [a] -> [a]
omitElementAt i list = let (left, right) = splitAt i list in left ++ (tail right)

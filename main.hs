module Main where

import Jacobi
import ConjugateGradient

import System.Environment

-- Test data

-- Wikipedia example
-- answers:
-- x1 = [5, 1.143]
-- x2 = [4.929, -1.713]
-- x25 = [7.111, -3.222]

a  = [ [2, 1]
     , [5, 7]
     ]
b  = [11, 13]
x = [1, 1]

x' = [0, 0, 0]
a' = [ [2   , 1 , 0.95]
     , [1   , 2 , 1   ]
     , [0.95, 1 , 2   ]
     ]
b' = [3.95, 4, 3.95]

jacobiDemo :: Double -> Int -> IO ()
jacobiDemo err limit = do
    let jacobiApproximations = take limit $ jacobiApproximationsUntilPrecise err a b x
        size = length jacobiApproximations
        jacobiApproximation = last jacobiApproximations
    putStrLn "Jacobi method"
    putStrLn $ show size ++ " iterations: " ++ show jacobiApproximation

conjugateGradientDemo :: Double -> Int -> IO ()
conjugateGradientDemo err limit = do
    let cgApproximations = take limit $ cgApproximationsUntilPrecise err a' b' x'
        size = length cgApproximations
        cgApproximation = last cgApproximations
    putStrLn "Conjugate gradient method"
    putStrLn $ show size ++ " iterations: " ++ show cgApproximation

main :: IO ()
main = do
    (errStr:limitStr:fileName:_) <- getArgs
    let err = read errStr
        limit = read limitStr
    jacobiDemo err limit
    conjugateGradientDemo err limit


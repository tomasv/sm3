module Main where

import Jacobi
import ConjugateGradient

import System.Environment

-- Test data format: (A, B, x0)

-- Wikipedia example for jacobi answers:
-- x1 = [5, 1.143]
-- x2 = [4.929, -1.713]
-- x25 = [7.111, -3.222]

type Example = ([[Double]], [Double], [Double])
type Method = Double -> [[Double]] -> [Double] -> [Double] -> [[Double]]

methodDemo :: Method -> String -> Double -> Int -> Example -> IO ()
methodDemo method name err limit (a, b, x) = do
    let approximations = take limit $ method err a b x
        size = length approximations
        approximation = last approximations
    putStrLn name
    putStrLn $ show size ++ " iterations: " ++ show approximation

-- takes three arguments when executing
-- precision, iteration limit and the file to load data from
-- data file format is simply the Example type in haskell notation
main :: IO ()
main = do
    (errStr:limitStr:fileName:_) <- getArgs
    contents <- readFile fileName
    let err = read errStr
        limit = read limitStr
        example = read contents
    methodDemo (jacobiApproximationsUntilPrecise) "Jacobi method" err limit example
    methodDemo (cgApproximationsUntilPrecise) "Conjugate gradient method" err limit example


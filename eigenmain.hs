
module Main where

import System.Environment
import Eigen


main = do
    [err, limit, a, b] <- getArgs
    let l = lambdaApproximation (read a) (read b)
        a = []
        x = [1, 1, 1, 1]
        lst = take (read limit) $ lambdaSeriesWithPrecision (read err) l a x
    putStrLn $ show (length lst) ++ " iterations: " ++ show (last lst)




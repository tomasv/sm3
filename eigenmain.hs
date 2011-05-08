module Main where

import System.Environment
import Eigen
import Datatypes
import Text.Printf (printf)

processing = do
    (err : limit : a : b : fileName : _) <- getArgs
    contents <- readFile fileName
    let lambda = lambdaApproximation (read a) (read b)
        (m, x) = (read contents)
        results = take (read limit) $ approximationsUntilPrecise (read err) lambda m x
        iterations = length results
        result = last results
        (eigenvalue, eigenvector, err1, err2) = (\(EigenResult a b c d) -> (a, b, c, d)) result
    printf "%d iterations, eigenvalue: %f, eigenvector: %s\n  err1: %f\n  err2: %f\n"
      iterations eigenvalue (show eigenvector) err1 err2

handler e = getProgName >>= printf "Usage: %s <precision> <iteration-limit> <a> <b> <filename>\n"

main = do
    processing `catch` handler




module Main where

import System.Environment

import qualified Jacobi as J
import qualified ConjugateGradient as CG
import Datatypes

import MatrixVector

showResult :: Result -> Int -> String
showResult (JacobiResult approximation e1 e2) size = do
    (show size) ++ " iterations: " ++ (show approximation) ++
      "\n  error: " ++ (show e1) ++ "\n  residual: " ++ (show e2)
showResult (CgResult approximation e1 e2) size = do
    (show size) ++ " iterations: " ++ (show approximation) ++
      "\n  error: " ++ (show e1) ++ "\n  residual: " ++ (show e2)

-- Test data format: (A, B, x0)

type Example = ([Vector], Vector, Vector)
type Method = Double -> Matrix -> Vector -> Vector -> Either String Results

methodDemo :: Method -> String -> Double -> Int -> Example -> IO ()
methodDemo method name err limit (a, b, x) = do
    putStrLn name
    case method err a b x of
         Left errorMessage -> putStrLn errorMessage
         Right results -> do
            let size = length results
                result = last results
            putStrLn $ showResult result size

-- takes three arguments when executing
-- precision, iteration limit and the file to load data from
-- data file format is simply the Example type in haskell notation
demo :: IO ()
demo = do
    (errStr:limitStr:fileName:_) <- getArgs
    contents <- readFile fileName
    let err = read errStr
        limit = read limitStr
        example = read contents
    methodDemo (J.approximationsUntilPrecise) "Jacobi method" err limit example
    methodDemo (CG.approximationsUntilPrecise) "Conjugate gradient method" err limit example

handler :: IOError -> IO ()
handler e = do
    name <- getProgName
    putStrLn $ "Usage: " ++ name ++ " <error> <iteration-limit> <filename>"

main :: IO ()
main = do
    demo `catch` handler

module Main where

import System.Environment

import qualified Simpson as S
import qualified Gauss as G

function :: Double -> Double
function x = (sqrt(x) - 1)^2

simpsonDemo :: Double -> Int -> IO ()
simpsonDemo precision limit = do
    let results = S.runSeriesP precision (function) 1 2 2
    putStrLn "Simpson:"
    putStrLn $ unlines (map (show) results)

gaussDemo :: Double -> Int -> IO ()
gaussDemo precision limit = do
    let results = G.runSeriesP precision (function) 1 2 2
    putStrLn "Gauss:"
    putStrLn $ unlines (map (show) results)

main = do
    (precisionString:limitString:[]) <- getArgs
    let precision = (read precisionString)
        limit = (read limitString)
    simpsonDemo precision limit
    gaussDemo precision limit


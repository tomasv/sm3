module Datatypes where

import MatrixVector

type Results = [Result]
data Result = JacobiResult Vector Double Double |
              CgResult Vector Double Vector |
              EigenResult Double Vector Double Double |
              DResult Double Double Double Double

instance Show Result where
    show (DResult a err n diff) = (show a) ++ " err: " ++ (show err) ++ " n: " ++ (show n) ++ " diff: " ++ (show diff)

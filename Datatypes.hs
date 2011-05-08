module Datatypes where

import MatrixVector

type Results = [Result]
data Result = JacobiResult Vector Double Double | CgResult Vector Double Vector

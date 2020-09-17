module Pythag where

pythag :: Int -> [(Int, Int, Int)]
pythag n = [(x, y, z) | x <- [1..n], y <- [x..n], z <- [x..n], x^2 + y^2 == z^2]
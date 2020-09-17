module Main where

primes :: [Int]
primes = sieve [2..]

sieve :: [Int] -> [Int]
sieve (p : xs) = p : filter (\x -> x `mod` p /= 0) xs

main :: IO ()
main = do ns <- getLine
          let n = read ns
              ps = take n primes in
              do sequence_ [putStr $ show p ++ " " | p <- ps]
              
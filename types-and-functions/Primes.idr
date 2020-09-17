module Main 

import Data.Strings
import Data.Stream

filter : (a -> Bool) -> Stream a -> Stream a
filter p (x :: xs) = if p x 
                        then x :: filter p xs
                        else filter p xs

sieve : Stream Int -> Stream Int
sieve (p :: xs) = p :: filter (\x => x `mod` p /= 0) xs

primes : Stream Int
primes = sieve (iterate (+1) 2)

main : IO ()
main = do ns <- getLine
          let n = stringToNatOrZ ns
          let ps = take n primes
          sequence_ [putStr $ show p ++ " " | p <- ps]
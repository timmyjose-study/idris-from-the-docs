-- run as: $ idris2 -p calc CalcClient.idr

module Main

import Data.Strings

import Calc.Add
import Calc.Sub
import Calc.Mul
import Calc.Div

readNum : HasIO io => io (Maybe Int)
readNum = do putStr "Enter a number: "
             ns <- getLine
             case all isDigit (unpack ns) of
                  False => pure Nothing
                  True => pure $ Just (cast ns)

main : HasIO io => io ()
main = do Just n <- readNum
            | Nothing => do putStrLn "First number is invalid"
          Just m <- readNum
            | Nothing => do putStrLn "Second number is invalid"
          let sum = add n m 
          let diff = subtract n m
          let prod = multiply n m
          let quot = divide n m
          putStrLn $ "Sum = " ++ show sum ++ " , difference = " ++ show diff 
                    ++ ", product = " ++ show prod ++ ", quotient = " ++ show quot
                              


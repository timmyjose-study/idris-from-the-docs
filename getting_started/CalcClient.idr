-- build instructions:
-- $ cd calc && idris2 --build calc.ipkg && idris2 --install calc.ipkg
-- $ cd .. && idris2 --package calc CalcClient.idr
---
module Main

import Data.Strings
import Calc

%default total

readNum : HasIO io => io (Maybe Int)
readNum = do putStr "Enter a number: "
             ns <- getLine
             case all isDigit (unpack ns) of
                  False => pure Nothing
                  True => pure $ Just (cast ns)

main : HasIO io => io ()
main = do Just n <- readNum 
              | Nothing => do putStrLn "Error: first number is invalid"
          Just m <- readNum
              | Nothing => do putStrLn "Error: second number is invalid"
          let sum = addInts n m 
          let diff = subInts n m
          let prod = mulInts n m
          let quot = divInts n m
          putStrLn $ "Sum = " ++ show sum
                      ++ ", Diff = " ++ show diff
                      ++ ", Prod = " ++ show prod
                      ++ ", Quot = " ++ show quot
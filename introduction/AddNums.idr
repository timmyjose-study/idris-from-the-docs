module Main

import Data.Strings

%default total

readNum : HasIO io => io (Maybe Nat)
readNum = do putStr "Enter a number: "
             ns <- getLine
             case all isDigit (unpack ns) of
                  False => pure Nothing
                  True => pure $ Just (stringToNatOrZ ns)

addNums : HasIO io => io (Maybe Nat)
addNums = do Just n <- readNum
              | Nothing => do putStrLn "Error: first input is not a number"
                              pure Nothing
             Just m <- readNum
               | Nothing => do putStrLn "Error: second input is not a number"
                               pure Nothing
             pure $ Just (n + m)

main : HasIO io => io ()
main = do Just sum <- addNums
            | Nothing => pure ()
          printLn sum

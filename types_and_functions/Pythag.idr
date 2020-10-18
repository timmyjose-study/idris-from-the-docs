module Pythag

import Data.Strings

%default total

pythag : Int -> List (Int, Int, Int)
pythag n = [(x, y, z) | z <- [1..n], y <- [1..z], x <- [1..y], x * x + y * y == z * z]

readNum : HasIO io => io (Maybe Int)
readNum = do ns <- getLine
             case all isDigit (unpack ns) of
                  False => pure Nothing
                  True => pure $ Just (cast ns)

main : HasIO io => io ()
main = do Just n <- readNum
            | Nothing => do putStrLn "Error: you should have entered a number!"
                            pure ()
          let res = pythag n
          printLn res
                        

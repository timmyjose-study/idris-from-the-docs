module Main

import Control.App

printSquare : Has [Num, Show] a => a -> IO ()
printSquare x = printLn (x * x)

main : IO ()
main = printSquare 10
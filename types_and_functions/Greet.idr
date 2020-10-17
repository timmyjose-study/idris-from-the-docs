module Greet

%default total

greet : HasIO io => io ()
greet = do putStr "What is your name? "
           name <- getLine
           putStrLn $ "Nice to meet you again, " ++ name

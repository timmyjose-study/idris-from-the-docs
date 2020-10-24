module Main

import Data.Strings

%default total

%foreign "C:add,libcalc"
add : Int -> Int -> Int

%foreign "C:sub,libcalc"
sub : Int -> Int -> Int

%foreign "C:mul,libcalc"
mul : Int -> Int -> Int

%foreign "C:div,libcalc"
div : Int -> Int -> Int

%foreign "C:op_with_message,libcalc"
prim_opWithMessage : String -> (Int -> Int -> Int) -> Int -> Int -> PrimIO Int

opWithMessage : HasIO io => String -> (Int -> Int -> Int) -> Int -> Int -> io Int
opWithMessage s f x y = primIO $ prim_opWithMessage s f x y

readNum : HasIO io => io (Maybe Int)
readNum = do putStr "Enter number: "
             ns <- getLine
             case all isDigit (unpack ns) of
                  False => pure Nothing
                  True => pure (Just (cast ns))


main : IO ()
main = do Just x <- readNum 
            | Nothing => putStrLn "first number is invalid"
          Just y <- readNum
            | Nothing => putStrLn "second number is invalid"
          printLn (add x y)
          printLn (sub x y)
          printLn (mul x y)
          printLn (Main.div x y)
          opWithMessage "sum" add x y
          opWithMessage "diff" sub x y
          opWithMessage "prod" mul x y
          opWithMessage "quot" Main.div x y
          pure ()

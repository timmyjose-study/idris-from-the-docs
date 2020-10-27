module Main

import Data.Strings

%default total

libsmall : String -> String
libsmall fn = "C:" ++ fn ++ ",libsmall"

%foreign (libsmall "add")
prim_add : Int -> Int -> PrimIO Int

add : HasIO io => Int -> Int -> io Int
add x y = primIO $ prim_add x y

%foreign (libsmall "sub")
prim_sub : Int -> Int -> PrimIO Int

sub : HasIO io => Int -> Int -> io Int
sub x y = primIO $ prim_sub x y

%foreign (libsmall "mul")
prim_mul : Int -> Int -> PrimIO Int

mul : HasIO io => Int -> Int -> io Int
mul x y = primIO $ prim_mul x y

%foreign (libsmall "divide")
prim_div : Int -> Int -> PrimIO Int

div : HasIO io => Int -> Int -> io Int
div x y = primIO $ prim_div x y

%foreign (libsmall "add_with_message")
prim_addWithMessage : String -> Int -> Int -> PrimIO ()

addWithMessage : HasIO io => String -> Int -> Int -> io ()
addWithMessage m x y = primIO $ prim_addWithMessage m x y

%foreign (libsmall "sub_with_message")
prim_subWithMessage : String -> Int -> Int -> PrimIO ()

subWithMessage : HasIO io => String -> Int -> Int -> io ()
subWithMessage m x y = primIO $ prim_subWithMessage m x y

%foreign (libsmall "mul_with_message")
prim_mulWithMessage : String -> Int -> Int -> PrimIO ()

mulWithMessage : HasIO io => String -> Int -> Int -> io ()
mulWithMessage m x y = primIO $ prim_mulWithMessage m x y

%foreign (libsmall "div_with_message")
prim_divWithMessage : String -> Int -> Int -> PrimIO ()

divWithMessage : HasIO io => String -> Int -> Int -> io ()
divWithMessage m x y = primIO $ prim_divWithMessage m x y

readInt : HasIO io => io (Maybe Int)
readInt = do input <- getLine
             case all isDigit (unpack input) of
                  False => pure Nothing
                  True => pure $ Just (cast input)

main : IO ()
main = do Just x <- readInt 
            | Nothing => putStrLn "first number is invalid"
          Just y <- readInt
            | Nothing => putStrLn "second number is invalid"
          sum <- add x y
          printLn sum
          addWithMessage "sum" x y
          subWithMessage "sub" x y
          mulWithMessage "mul" x y
          divWithMessage "div" x y

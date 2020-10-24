module Main

-- syntax: Language:name,library

%foreign "C:add,libsmall"
add : Int -> Int -> Int

-- primIO : HasIO io => PrimIO a => io a

%foreign "C:add_with_message,libsmall"
prim_addWithMessage : String -> Int -> Int -> PrimIO Int

addWithMessage : HasIO io => String -> Int -> Int -> io Int
addWithMessage s x y = primIO $ prim_addWithMessage s x y

main : IO ()
main = do printLn (add 12 13)
          addWithMessage "Sum" 12 13
          pure ()


module Puts

%default total

%foreign "C:puts,libc"
puts : String -> PrimIO ()

myPuts : HasIO io => String -> io ()
myPuts s = primIO $ puts s

main : IO ()
main = myPuts "Hello, world!\n"
module Main

import System.FFI

%default total

libsmall : String -> String
libsmall fn = "C:" ++ fn ++ ",libsmall"

Point : Type
Point = Struct "point" [("x", Int), ("y", Int)]

%foreign (libsmall "mk_point")
mkPoint : Int -> Int -> Point

%foreign (libsmall "free_point")
prim_freePoint : Point -> PrimIO ()

freePoint : HasIO io => Point -> io ()
freePoint p = primIO $ prim_freePoint p

showPoint : Point -> String
showPoint p = 
  let x : Int = getField p "x"
      y : Int = getField p "y" in
      show (x, y)

main : IO ()
main = do let p = mkPoint 100 200
          putStrLn (showPoint p)
          setField p "x" (the Int 42)
          setField p "y" (the Int (-42))
          putStrLn (showPoint p)
          freePoint p


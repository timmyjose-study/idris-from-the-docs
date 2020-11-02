module Main

import Control.App
import Control.App.Console

{-
  App is defined as:

  data App: {default MayThrow l : Path} -> (es : List Error) -> Type -> Type

  where `Error` is a synonym for `Type`.
-}

hello : Console es => App es ()
hello = putStrLn "Hello, world"

main : IO ()
main = run hello
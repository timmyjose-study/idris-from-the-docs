module Main

%default total

%foreign "C:apply_fn,libsmall"
prim_applyFn : String -> Int -> (String -> Int -> String) -> PrimIO String

applyFn : HasIO io => String -> Int -> (String -> Int -> String) -> io String
applyFn s x f = primIO $ prim_applyFn s x f

pluralise : String -> Int -> String
pluralise str x = 
  show x ++ " " ++
    if x == 1 
       then str
       else str ++ "s"

%foreign "C:apply_fn,libsmall"
prim_applyFnIO : String -> Int -> (String -> Int -> PrimIO String) -> PrimIO String

-- toPrim : IO a -> PrimIO a
applyFnIO : HasIO io => String -> Int -> (String -> Int -> IO String) -> io String
applyFnIO s x f = primIO $ prim_applyFnIO s x (\s, d => toPrim $ f s d)

pluraliseWithMessage : String -> Int -> IO String
pluraliseWithMessage str x = 
  do putStrLn "pluralising"
     pure $ show x ++ " " ++
            if x == 1
               then str
               else str ++ "s"

main : IO ()
main = do str1 <- applyFn "Biscuit" 10 pluralise
          putStrLn str1
          str2 <- applyFn "Tree" 1 pluralise
          putStrLn str2
          str3 <- applyFnIO "Human" 5 pluraliseWithMessage
          putStrLn str3
          str4 <- applyFnIO "Dog" 1 pluraliseWithMessage
          putStrLn str4

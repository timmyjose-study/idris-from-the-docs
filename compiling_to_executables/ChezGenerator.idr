module Main

import Data.Strings

%default total

-- the definitions are in calc.scm, run the program as:
-- $ idris2 -o Chez --directive extranRuntime=calc.scm ChezGenerator.idr

%foreign "scheme:my-add"
myAdd : Int -> Int -> Int

%foreign "scheme:my-sub"
mySub : Int -> Int -> Int

%foreign "scheme:my-mul"
myMul : Int -> Int -> Int

%foreign "scheme:my-div"
myDiv : Int -> Int -> Int

readNum : HasIO io => io (Maybe Int)
readNum = do input <- getLine
             case all isDigit (unpack input) of
                  False => pure Nothing
                  True => pure $ Just (cast input)

main : HasIO io => io ()
main = do Just x <- readNum
            | Nothing => do putStrLn "First number is invalid"
          Just y <- readNum
            | Nothing => do putStrLn "Second number is invalid"
          putStrLn $ "Sum = " ++ show (myAdd x y) ++ ", diff = " ++ show (mySub x y) ++
                            ", prod = " ++ show (myMul x y) ++ ", quot = " ++ show (myDiv x y)
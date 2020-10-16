module Calc

%default total

public export
addInts : Int -> Int -> Int
addInts x y = x + y

public export
subInts : Int -> Int -> Int
subInts x y = x - y

public export
mulInts : Int -> Int -> Int
mulInts x y = x * y

public export
divInts : Int -> Int -> Int
divInts x y = x `div` y

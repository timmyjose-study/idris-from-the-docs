module Calc.Div

%default total

export
divide : Int -> Int -> Int
divide x y = if y == 0 
             then 0
             else x `div` y

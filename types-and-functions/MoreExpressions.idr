module MoreExpressions

import Data.List
import Data.Strings

%default total

-- case expressions

partial
mySplitAt : Char -> String -> (String, String)
mySplitAt c cs = case break (== c) cs of
                      (x, y) => (x, strTail y)

listLookup : Nat -> List a -> Maybe a 
listLookup _ Nil = Nothing
listLookup Z (x :: _) = Just x
listLookup (S k) (_ :: xs) = listLookup k xs

lookupDefault : Nat -> List a -> a -> a
lookupDefault n xs d = case listLookup n xs of
                            Nothing => d
                            Just x => x

-- list comprehensions

pythag : Int -> List (Int, Int, Int)
pythag n = [(x, y, z) | z <- [1..n], y <- [1..z], x <- [1..y], x * x + y * y == z * z]

-- let bindings

mirror : List a -> List a
mirror xs = let xs' = reverse xs in
                xs ++ xs'

data Person = MkPerson String Int

showPerson : Person -> String
showPerson p = let MkPerson name age = p in
                   name ++ " is " ++ show age ++ " years old"

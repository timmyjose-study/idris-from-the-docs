module Reverse

myReverse : List a -> List a
myReverse xs = aux [] xs where
  aux : List a -> List a -> List a
  aux acc [] = acc
  aux acc (x :: xs) = aux (x :: acc) xs
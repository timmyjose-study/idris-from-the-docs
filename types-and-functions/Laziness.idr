module Laziness

import Data.Stream

{-
    data Lazy : Type -> Type where
      Lazy : (val : a) -> Lazy a

    Force : Lazy a -> a

-}

ifThenElse : Bool -> Lazy a -> Lazy a -> a
ifThenElse True t e = t
ifThenElse False t e = e

-- Stream is just one kind of a potentially infinite data structure
{-
  data Stream : Type -> Type where
    (::) : (e : a) -> Inf (Stream a) -> Stream a

-}

ones : Stream Nat
ones = 1 :: ones

filter : (a -> Bool) -> Stream a -> Stream a
filter p (x :: xs) = if p x 
                        then x :: filter p xs
                        else filter p xs

partial
sieve : Stream Int -> Stream Int
sieve (p :: xs) = p :: filter (\x => x `mod` p /= 0) xs

primes : Stream Int
primes = sieve (iterate (+1) 2)

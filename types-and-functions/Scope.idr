module Scope

%default total

data MyLT = Yes | No

foo : Int -> Int
foo x = case isLT of
             Yes => x * 2
             No => x * 4
  where
    isLT : MyLT
    isLT = if x < 20 then Yes else No

bar : Int -> Int
bar x = case isGT of
             Da => x * 2
             Nyet => x * 4
  where
    data MyGT = Da | Nyet

    isGT : MyGT
    isGT = if x > 100 then Da else Nyet

even : Nat -> Bool
even Z = True
even (S k) = odd k where
  odd : Nat -> Bool
  odd Z = False
  odd (S k) = even k

test : List Nat
test = [c (S 2), c Z, d (S Z)] where
  c : Nat -> Nat
  c x = 42 + x

  d : Nat -> Nat
  d y = c (y + 1 + z y) where
    z : Nat -> Nat
    z w = y + w
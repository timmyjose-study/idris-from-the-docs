module TheoremProving

%default total

-- equality
{-
  data Equal : a -> b -> Type where
    Refl : Equal x x

  Equal x y is also written as x = y
-}

fiveIsFive : Equal 5 5
fiveIsFive = Refl

twoPlusTwoIsFour : 2 + 2 = 4
twoPlusTwoIsFour = Refl

-- the Empty type - Void, useful for 
-- proving impossible cases in theorem proving.
-- Essentially, proof by contradiction

-- proof that zero can never be equal to a successor
disjoint : (n : Nat) -> Z = S n -> Void
disjoint n prf = replace { p = disjointTy } prf () where
  disjointTy : Nat -> Type
  disjointTy Z = ()
  disjointTy (S k) = Void

-- proving theorems
-- a proof in Idris is basically a program with a precise enough type to guarantee a special property of interest
-- Hence we write proofs the same way as other programs.

plusReduces : (n : Nat) -> plus Z n = n
plusReduces n = Refl

plusReducesZ : (n : Nat) -> n = plus n Z
plusReducesZ Z = Refl
plusReducesZ (S k) = cong S (plusReducesZ k)

plusReducesS : (n : Nat) -> (m : Nat) -> S (plus n m) = plus n (S m)
plusReducesS Z m = Refl
plusReducesS (S k) m = cong S (plusReducesS k m)
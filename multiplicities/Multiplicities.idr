module Multiplicities

import Data.Vect

append : Vect n a -> Vect m a -> Vect (n + m) a
append [] ys = ys
append (x :: xs) ys = x :: append xs ys

{-
  Idris is based on QTT, which uses Linear Logic. As such, Idris supports three levels of "multiplicities":
    - 0 meaning that the value is erased at runtime (this applies to implicits as well)
    - 1 meaning that the value is consumed exactly once at runtime, and
    - no multiplicity annotation, meaning unrestrained multiplicity.
-}

-- identical to the definition above, just explicit
appendExplicit : { 0 n : Nat } -> { 0 m : Nat } -> Vect n a -> Vect m a -> Vect (n + m) a
appendExplicit [] ys = ys
appendExplicit (x :: xs) ys = x :: appendExplicit xs ys

-- linearity

-- this is not possible
--duplicate : (1 _ : a) -> (a, a)
--duplicate x = (x, x)

data Lin : Type -> Type where
  MkLin : (1 _ : a) -> Lin a

getLin : (1 _ : Lin a) -> a
getLin (MkLin x) = x

data Unr : Type -> Type where
  MkUnr : a -> Unr a

getUnr : Unr a -> a
getUnr (MkUnr x) = x

-- erasure

vlen :  { n : _ } -> Vect n a -> Nat
vlen xs = n

sumLengths : { m, n : _ } -> Vect m a -> Vect n a -> Nat
sumLengths xs ys = m + n

-- it is an error to try pattern matching on an erased argument (i.e., an argument with multiplicity 0), unless
-- that argument's value can be uniquely inferred from another argument

-- so this does not work
--badNot : (0 x : Bool) -> Bool
--badNot False = True
--badNot True = False

-- but this works since the dependent argument (SBool x) allows us to uniquely infer a value for the first argument

data SBool : Bool -> Type where
  SFalse: SBool False
  STrue : SBool True

sNot : (0 x : Bool) -> SBool x -> Bool
sNot False SFalse = True
sNot True STrue = False
sNot False STrue impossible
sNot True SFalse impossible

-- pattern matching on types

showType : Type -> String
showType Int = "Int"
showType (List a) = "List of " ++ showType a
showType (Vect _ a) = "Vect of " ++ showType a
showType (Nat -> a) = "Function from Nat to " ++ showType (a Z)
showType _ = "something else"

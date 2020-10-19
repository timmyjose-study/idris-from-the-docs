module NamedImplementations 

import Data.List

%default total

-- named implementations allow us to have multiple implementations for a particular interface
-- for a particular type.

{-
  [name] Interface Type where
    func

  and then use as: func @{name} instead of the normal func
-}

[myord] Ord Nat where
  compare Z (S k) = GT
  compare (S k) Z = LT
  compare Z Z = EQ
  compare (S k) (S j) = compare @{myord} k j

testList : List Nat
testList = [3, 4, 1, 2, 3, 5, 4, 4, 5]

normalSort : List Nat -> List Nat
normalSort = sort

reverseSort : List Nat -> List Nat
reverseSort = sort @{myord} -- use the custom interface implementation with the name `myord`

-- this is something that Haskell cannot do - and the reason why it has to define wrapper classes
-- like `Sum` and `Product` in the Data.Monoid module. Named Implementations provides an elegant way
-- out of this boilerplate.

{-
  interface Semigroup ty where
    (<+>) : ty -> ty -> ty

  interface Semigroup ty => Monoid ty where
    neutral : ty
-}

[PlusNatSemi] Semigroup Nat where
  (<+>) x y = x + y

[MultNatSemi] Semigroup Nat where
  (<+>) x y = x * y

[PlusNatMonoid] Monoid Nat using PlusNatSemi where -- note the `using` clause
  neutral = 0

[MultNatMonoid] Monoid Nat using MultNatSemi where 
  neutral = 1
module Monoids

%default total

{-
  interface Semigroup ty where
    (<+>) : ty -> ty -> ty

  interface Semigroup ty => Monoid ty where
    neutral : ty
-}

-- lets' define IntS as Monoids under both addition and multiplication

[PlusIntSemi] Semigroup Int where
  (<+>) x y = x + y

[PlusIntMonoid] Monoid Int using PlusIntSemi where
  neutral = 0

[MultIntSemi] Semigroup Int where
  (<+>) x y = x * y

[MultIntMonoid] Monoid Int using MultIntSemi where
  neutral = 1

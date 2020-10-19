module Monoids where

  {-
    class Monoid a where
      mempty :: a
      mappend :: a -> a -> a

      mconcat :: [a] -> a
      mconcat = foldr mappend mempty
  -}

-- since {Z, +, 0} and {Z, *, 1} form monoids, in order to declare these as Monoids, we
-- have to use wrapper types in Haskell. In Idris, we can simply used "named implementation"S.
-- See Monoids.idr

newtype Sum a = Sum a 
  deriving (Eq, Ord, Read, Show)

getSum :: Sum a -> a
getSum (Sum x) = x

instance Num a => Semigroup (Sum a) where
  (<>) (Sum x) (Sum y) = Sum (x + y)

instance Num a => Monoid (Sum a) where
  mempty = Sum 0
  mappend (Sum x) (Sum y) = Sum (x + y)

newtype Prod a = Prod a
  deriving (Eq, Ord, Read, Show)

getProd :: Prod a -> a 
getProd (Prod a) = a

instance Num a => Semigroup (Prod a) where
  (<>) (Prod x) (Prod y) = Prod (x * y)

instance Num a => Monoid (Prod a) where
  mempty = Prod 1
  mappend (Prod x) (Prod y) = Prod (x * y)
module DepPairs

import Data.Vect

%default total

--data Vect : Nat -> Type -> Type where
--  Nil : Vect Z a
--  (::) : a -> Vect k a -> Vect (S k) a
--
data DepPair : (a : Type) -> (p : a -> Type) -> Type where
  MkDepPair : { p : a -> Type } -> (x : a) -> p x -> DepPair a p

vi : Vect 10 Nat
vi = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

mutual
  even : Nat -> Bool
  even Z = True
  even (S k) = odd k where

  odd : Nat -> Bool
  odd Z = False
  odd (S k) = even k

vFilter : (a -> Bool) -> Vect n a -> (p : Nat ** Vect p a)
vFilter _ [] = (0 ** [])
vFilter p (x :: xs) = case p x of
                           False => vFilter p xs
                           True => let (_ ** res) = vFilter p xs in
                                       (_ ** (x :: res))

vFilter' :  { a: _ } -> (a -> Bool) -> Vect n a -> DepPair Nat (\p => Vect p a)
vFilter' _ [] = MkDepPair 0 []
vFilter' p (x :: xs) = case p x of
                            False => vFilter' p xs
                            True => let (MkDepPair _ res) = vFilter' p xs in
                                        MkDepPair _ (x :: res)



module InteractiveEditing

import Data.Vect

%default total

%name Vect xs, ys, zs

{-
  <LocalLeader> a -> add initial clause
  <LocalLeader> c -> case split
  <LocalLeader> m -> add missing clause(s)
  <LocalLeader> s -> proof search
  <LocalLeader> w -> add with clause



-}

vzipwith : (a -> b -> c) -> Vect n a -> Vect n b -> Vect n c
vzipwith f [] [] = []
vzipwith f (x :: xs) (y :: ys) = f x y :: vzipwith f xs ys

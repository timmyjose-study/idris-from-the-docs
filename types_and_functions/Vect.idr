module Vect 

%default total

data Vect : Nat -> Type -> Type where
  Nil : Vect Z a
  (::) : a -> Vect k a -> Vect (S k) a

(++) : Vect n a -> Vect m a -> Vect (n + m) a
(++) Nil ys = ys
(++) (x :: xs) ys = x :: xs ++ ys

-- finite sets

data Fin : Nat -> Type where
  FZ : Fin (S k)
  FS : Fin k -> Fin (S k)

--index : Fin n -> Vect n a -> a
index : forall a, n . Fin n -> Vect n a -> a
index FZ (x :: _) = x
index (FS k) (_ :: xs) = index k xs

natToFin : Nat -> (n : Nat) -> Maybe (Fin n)
natToFin Z (S _) = Just FZ
natToFin (S k) (S j) = 
  case natToFin k j of
       Nothing => Nothing
       Just k' => Just (FS k')
natToFin _ _ = Nothing

vs : Vect 5 String
vs = ["hello", "world", "nice", "to", "meet"]

elementAt : { n : _ } -> Nat -> Vect n a -> Maybe a
elementAt idx vec = case natToFin idx n of
                         Nothing => Nothing
                         Just idx' => Just $ index idx' vec

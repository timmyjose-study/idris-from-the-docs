module DependentTypes

%default total

ISSingleton : Bool -> Type
ISSingleton True = Nat
ISSingleton False = List Nat

mkSingle : (b : Bool) -> ISSingleton b
mkSingle True = 100
mkSingle False = [1, 2, 3, 4, 5]

sum : (b : Bool) -> ISSingleton b -> Nat
sum True x = x
sum False [] = 0
sum False (n :: ns) = n + sum False ns

infixr 10 ::

data Vect : Nat -> Type -> Type where
  Nil : Vect Z a
  (::) : a -> Vect n a -> Vect (S n) a

app : Vect n a -> Vect m a -> Vect (n + m) a
app [] ys = ys
app (x :: xs) ys = x :: app xs ys

data Fin : Nat -> Type where
  FZ : Fin (S k)
  FS : Fin k -> Fin (S k)

index : forall a, n . Fin n -> Vect n a -> a
index FZ (x :: xs) = x
index (FS k) (_ :: xs) = index k xs

mutual
  even : Nat -> Bool
  even Z = True
  even (S k) = odd k

  odd : Nat -> Bool
  odd Z = False
  odd (S k) = even k
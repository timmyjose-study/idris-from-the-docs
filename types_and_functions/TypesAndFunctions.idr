module TypesAndFunctions

%default total

-- data types

data MyNat = MZ | MS MyNat

mynat2int : MyNat -> Int
mynat2int MZ = 0
mynat2int (MS n) = 1 + mynat2int n

partial
int2mynat : Int -> MyNat
int2mynat 0 = MZ
int2mynat n = MS (int2mynat (n - 1))

plus : MyNat -> MyNat -> MyNat
plus MZ m = m
plus (MS n) m = MS (plus n m)

mult : MyNat -> MyNat -> MyNat
mult MZ _ = MZ
mult (MS n) m = plus m (mult n m)

infixr 10 ::

data MyList a = Nil | (::) a (MyList a)

Show a => Show (MyList a) where
  show Nil = "Nil"
  show (x :: xs) = "(Cons " ++ show x ++ " " ++ show xs ++ ")"

len : MyList a -> Nat
len Nil = Z
len (_ :: xs) = S (len xs)

fromList : List a -> MyList a
fromList [] = Nil
fromList (x :: xs) = (::) x (fromList xs)

-- where clauses

myReverse : List a -> List a
myReverse xs = aux xs [] where
  aux : List a -> List a -> List a
  aux [] acc = acc
  aux (x :: xs) acc = aux xs (x :: acc)

-- till https://github.com/idris-lang/Idris2/issues/588 is fixed, this
-- definition has to be in the top-level
data MyLT = Yes | No

foo : Int -> Int
foo x = case isLT of
            Yes => x * 2
            No => x * 4 where
              isLT : MyLT
              isLT = if x < 10 then Yes else No

even : Nat -> Bool
even Z = True
even (S k) = odd k where
  odd : Nat -> Bool
  odd Z = False
  odd (S k) = even k

test : List Nat
test = [c (S 1), c Z, d (S Z)] where
  c : Nat -> Nat
  c x = 42 + x

  d : Nat -> Nat
  d y = c (y + 1 + z y) where
    z : Nat -> Nat
    z w = y + w

-- totality and covering

partial
from : Maybe a -> a
from (Just x) = x

-- dependent types

isSingleton : Bool -> Type
isSingleton False = List Nat
isSingleton True = Nat

mkSingle : (x : Bool) -> isSingleton x
mkSingle False = []
mkSingle True = 0

mysum : (single : Bool) -> isSingleton single -> Nat
mysum False [] = 0
mysum False (n :: ns) = n + mysum False ns
mysum True d = d

-- declaration order and mutual blocks

mutual
  myeven : Nat -> Bool
  myeven Z = True
  myeven (S k) = myodd k

  myodd : Nat -> Bool
  myodd Z = False
  myodd (S k) = myeven k

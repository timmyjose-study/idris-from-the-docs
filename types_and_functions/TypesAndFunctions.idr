module TypesAndFunctions

import Data.Vect

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

greet : HasIO io => io ()
greet = do putStr "What is your name? "
           name <- getLine
           putStrLn $ "Nice to meet you, " ++ name

-- laziness
{-
  data Lazy : Type -> Type where
    Delay : (val : a) -> Lazy a

  Force : Lazy a -> a
-}

myIfThenElse : Bool -> Lazy a -> Lazy a -> a
myIfThenElse True t e = t
myIfThenElse False t e = e

-- infinite data types (codata)

{-
  data Stream : Type -> Type where
    (::) : (e : a) -> Inf (Stream a) -> Stream a
-}

ones : Stream Nat
ones = 1 :: ones

-- useful data types

{-
  data List a = Nil | (::) a (List a)

  data Vect : Nat -> Type -> Type where
    Nil : Vect Z a
    (::) : a -> Vect k a -> Vect (S k) a
-}

{-
  map : (a -> b) -> List a -> List b
  map _ [] = []
  map f (x :: xs) = f x :: map f xs

  map : (a -> b) -> Vect n a -> Vect n b
  map _ [] = []
  map f (x :: xs) = f x :: map f xs
-}

intVec : Vect 5 Int
intVec = [1, 2, 3, 4, 5]

double : Int -> Int
double x = x + x

listLookup : Nat -> List a -> Maybe a
listLookup _ [] = Nothing
listLookup Z (x :: _) = Just x
listLookup (S k) (_ :: xs) = listLookup k xs

-- tuples

{-
  data Pair : Type -> Type -> Type where
    MkPair : a -> b -> Pair a b
-}

fred : (String, Nat)
fred = ("Fred", 42)

jim : (String, Int, String)
jim = ("Jim", 25, "Cambridge")

-- dependent pairs (Signma Types)

{-
  data DPair : (a : Type) -> (p : a -> Type) -> Type where
    MkDPair : {p : a -> Type} -> (x : a) -> p x -> DPair a p
-}

data DepPair : (a : Type) -> (p : a -> Type) -> Type where
  MkDepPair : { p : a -> Type } -> (x : a) -> p x -> DepPair a p

vec : (n : Nat ** Vect n Int)
vec = (3 ** [1, 2, 3])

vec1 : DPair Nat (\n => Vect n Int)
vec1 = MkDPair 3 [1, 2, 3]

vec2 : DepPair Nat (\n => Vect n Int)
vec2 = MkDepPair 3 [1, 2, 3]

vec3 : (n : Nat ** Vect n String)
vec3 = (_ ** ["Hello", "world"])

vec4 : (n ** Vect n Double)
vec4 = (_ ** [1.0, 2.2, -2.1212])

vFilter : (a -> Bool) -> Vect n a -> (p : Nat ** Vect p a)
vFilter p [] = (0 ** [])
vFilter p (x :: xs) = case p x of
                           False => vFilter p xs
                           True => let (_ ** res) = vFilter p xs in
                                       (_ ** (x :: res))

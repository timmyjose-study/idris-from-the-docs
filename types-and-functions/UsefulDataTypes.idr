module UsefulDataTypes

import Data.Vect

%default total

{-
  data List a = Nil | (::) a (List a)

  data Vect : Nat -> Type -> Type where
    Nil : Vect Z a
    (::) : a -> Vect k a -> Vect (S k) a 

-}

intVec : Vect 5 Int
intVec = [1, 2, 3, 4, 5]

double : Int -> Int
double x = x * 2

{-
  data Maybe a = Nothing | Just a

  maybe : Lazy b -> Lazy (a -> b) -> Maybe a -> b
-}

listLookup : Nat -> List a -> Maybe a
listLookup _ Nil = Nothing
listLookup Z (x :: _) = Just x
listLookup (S k) (_ :: xs) = listLookup k xs

-- tuples

{-
  data Pair a b = MkPair a b

-}

fred : (String, Int)
fred = ("Fred", 42)

jim : (String, Int, String)
jim = ("Jim", 25, "Cambridge")

third : Pair a (Pair b c) -> c -- same as (a, b, c)
third (_, _, c) = c

-- dependent pairs (Sigma types)

{-
  data DPair : (a : Type) -> (p : a -> Type) -> Type where
    MkDPair : { p : a -> Type } -> (x : a) -> p x -> DPair a p


  Syntacic sugar:

  (x : a ** p)
-}

vec : (n : Nat ** Vect n Int)
vec = (3 ** [1, 3, 4])

vec1 : DPair Nat (\n => Vect n Int)
vec1 = MkDPair 3 [1, 3, 4]

vec2 : (n ** Vect n Int)
vec2 = (_ ** [1, 2, 3])

myFilter : (elem -> Bool) -> Vect len elem -> (p ** Vect p elem)
myFilter p Nil = (_ ** [])
myFilter p (x :: xs) = 
  case myFilter p xs of
       (_ ** xs') => if p x then (_ ** x :: xs')
                            else (_ ** xs')

-- records

record Person where
  constructor MkPerson
  firstName, middleName, lastName : String
  age : Int

bob : Person
bob = MkPerson "Bob" "J." "Darper" 42

bobNextYear : Person
bobNextYear = record { age $= (+ 1) } bob

record Class where
  constructor ClassInfo
  students : Vect n Person
  className : String

addStudent : Person -> Class -> Class
addStudent p c = record { students = p :: students c } c

addStudent' : Person -> Class -> Class
addStudent' p c = record { students $= (p :: ) } c

record Name where
  constructor MkName
  firstName, middleName, lastName : String
  
record Employee where
  constructor MkEmployee
  name : Name
  age : Nat
  department : String

adam : Employee
adam = MkEmployee (MkName "Adam" "Xavier" "Leboy") 42 "Maintenance"

-- dependent records

record Prod a b where
  constructor Times
  fst : a
  snd : b

record SizedClass (size : Nat) where
  constructor SizedClassInfo
  students : Vect size Person
  className : String

addStudent'' : Person -> SizedClass n -> SizedClass (S n)
addStudent'' p c = record { students $= (p :: ) } c

record MyDPair a b where
  constructor MkMyDPair
  fst : a
  snd : b

cons : t -> (x : Nat ** Vect x t) -> (x : Nat ** Vect x t)
cons val xs = record { fst = S (fst xs), snd = (val :: snd xs) } xs

cons' : t -> (x : Nat ** Vect x t) -> (x : Nat ** Vect x t)
cons' val = record { fst $= S, snd $= (val ::) }

-- alpha substitution (?)
cons'' : t -> (x : Nat ** Vect x t) -> (y : Nat ** Vect y t)
cons'' val = record { fst $= S, snd $= (val ::) }

module Records 

import Data.Vect

%default total

record Person where
  constructor MkPerson
  firstName, middleName, lastName : String
  age : Int

bob : Person
bob = MkPerson "Bob" "H" "Fox" 42

dave : Person
dave = MkPerson "Dave" "H." "Chad" 49

record Class where
  constructor ClassInfo
  students : Vect n Person
  className : String

emptyClass : Class
emptyClass = ClassInfo [] ""

addStudent : Person -> Class -> Class
addStudent p c = record { students = p :: students c } c

addStudent' : Person -> Class -> Class
addStudent' p c = record { students $= (p ::) } c

changeClassName : String -> Class -> Class
changeClassName n c = record { className = n } c

record Employee where
  constructor MkEmployee
  id : Int
  person : Person

record Department where
  constructor MkDepartment
  id : Int
  manager : Employee
  employees : Vect n Employee

sales : Department
sales = MkDepartment 1 (MkEmployee 100 dave) [MkEmployee 1 (MkPerson "Susan" "L." "Weinberger" 23), MkEmployee 2 bob]

-- dependent records

record Prod a b where
  constructor Times
  fst : a
  snd : b

record SizedClass (size : Nat) where
  constructor SizedClassInfo
  students : Vect size Person
  className : String

addSizedStudent : Person -> SizedClass n -> SizedClass (S n)
addSizedStudent p c = record { students $= (p ::) } c

record Dpair a (p : a -> Type) where
  constructor MkDPair
  fst : a
  snd : p fst

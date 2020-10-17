module Records 

import Data.Vect

%default total

record Person where
  constructor MkPerson
  firstName, middleName, lastName : String
  age : Int

bob : Person
bob = MkPerson "Bob" "H" "Fox" 42

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

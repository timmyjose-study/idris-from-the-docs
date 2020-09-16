module Append

import Data.Vect

%default total

app : Vect n a -> Vect m a -> Vect (n + m) a
app [] ys = ys
app (x :: xs) ys = x :: app xs ys
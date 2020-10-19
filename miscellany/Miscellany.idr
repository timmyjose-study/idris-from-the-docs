module Miscellany

-- auto implicit arguments

-- this is the proof that the passed-in list is non-empty
isCons : List a -> Bool
isCons [] = False
isCons (_ :: _) = True

-- we can use that proof here, explicitly
safehead : (xs : List a)-> (isCons xs = True) -> a
safehead (x :: xs) _ = x

-- we can use the `auto` keyword to search for the proof automatically. Note the braces - 
-- we can explicitly supply the proof if we wish to.

bettersafehead : (xs : List a) -> { auto p : isCons xs = True } -> a
bettersafehead (x :: _) = x

-- default implicit arguments 

-- note that this case is not representative. The real value of default arguments is in Theorem Proving.

fibonacci : { default 0 lag : Nat } -> { default 1 lead : Nat } -> ( n : Nat ) -> Nat
fibonacci {lag} Z = lag
fibonacci {lag} {lead} (S n) = fibonacci {lag = lead} {lead = lag + lead} n
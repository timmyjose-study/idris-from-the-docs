module Interfaces 

import Data.List
import Data.Strings

%default total

data MyNat = MZ | MS MyNat 

Show MyNat where
  show MZ = "Z"
  show (MS k) = "(S " ++ show k ++ ")"

data Vect : Nat -> Type -> Type where
  Nil : Vect Z a
  (::) : a -> Vect k a -> Vect (S k) a

Show a => Show (Vect n a) where
  show xs = "[" ++ show' xs ++ "]" where
    show' : Vect m a -> String
    show' Nil = ""
    show' (x :: Nil) = show x
    show' (x :: xs) = show x ++ ", " ++ show' xs

-- default definitions

Eq MyNat where
  MZ == MZ = True
  (MS k) == (MS l) = k == l
  _ == _ = False

-- extending interfaces

sortAndShow : (Ord a, Show a) => List a -> String
sortAndShow = show . sort

-- functors and applicatives

{-
  interface Functor f where
    map : (a -> b) -> f a -> f b
-}

infixr 5 %%

data MyList a = Empty | (%%) a (MyList a)

infixr 5 @@

(@@) : MyList a -> MyList a -> MyList a
Empty @@ ys = ys
(x %% xs) @@ ys = x %% xs @@ ys

Show a => Show (MyList a) where
  show xs = "[" ++ show' xs ++ "]" where
    show' : MyList a -> String
    show' Empty = ""
    show' (x %% Empty) = show x
    show' (x %% xs) = show x ++ ", " ++ show' xs


Functor MyList where
  map f Empty = Empty
  map f (x %% xs) = f x %% map f xs

li : MyList Int
li = 1 %% 2 %% 3 %% 4 %% 5 %% Empty

{-
  infixl 2 <4>

  interface Functor f => Applicative f where
    pure : a -> f a
    (<*>) : f (a -> b) -> f a -> f b
-}

Applicative MyList where
  pure x = x %% Empty
  
  Empty <*> _ = Empty
  (f %% fs) <*> xs = map f xs @@ (fs <*> xs)

-- monads and the `do` notation

{-
  interface Applicative m => Monad m where
    (>>=) : m a -> (a -> m b) -> m b
-}

Monad MyList where
  Empty >>= f = Empty
  (x %% xs) >>= f = f x @@ (xs >>= f)

mAdd : Maybe Int -> Maybe Int -> Maybe Int
mAdd mx my = do x <- mx
                y <- my
                pure (x + y)

-- pattern-matching bind

readNumber : HasIO io => io (Maybe Int)
readNumber = do input <- getLine
                case all isDigit (unpack input) of
                     False => pure Nothing
                     True => pure $ Just (cast input)

readNumbers : HasIO io => io (Maybe (Int, Int))
readNumbers = do n <- readNumber
                 case n of
                      Nothing => pure Nothing
                      Just nn => do m <- readNumber
                                    case m of
                                         Nothing => pure Nothing
                                         Just mm => pure $ Just (nn, mm)

readNumbersBetter : HasIO io => io (Maybe (Int, Int))
readNumbersBetter = do Just n <- readNumber
                        | Nothing => pure Nothing
                       Just m <- readNumber
                        | Nothing => pure Nothing
                       pure $ Just (n, m)
                       
{-
  (!) : m a -> a

-}

mAddBetter : Maybe Int -> Maybe Int -> Maybe Int
mAddBetter mx my = pure (!mx + !my)

-- monad comprehensions - can be used for any type that implements both Monad and Alterantive.

{-
  interface Applicative f => Alternative f where
    empty : f a
    (<|>) : f a -> f a -> f a
-}

mAddAgain : Maybe Int -> Maybe Int -> Maybe Int
mAddAgain mx my = [x + y | x <- mx, y <- my]

{-
  interface Monad io => HasIO io where
    liftIO : (1 _ : IO a) -> io a
-}

-- idiom brackets

{-
  [| f a1 a2 .. an |] == pure f <*> a1 <*> a2 <*> ... <*> an
-}

applicativeAdd : Maybe Int -> Maybe Int -> Maybe Int
applicativeAdd mx my = pure (+) <*> mx <*> my

-- this can now be rewritten as

applicativeAddBetter : Maybe Int -> Maybe Int -> Maybe Int
applicativeAddBetter mx my = [| mx + my |]

{-
  Determining Parameters: When an interface has multiple parameters, but only some of the parameters are used to find implementations, we can mark with using
  the `| m` notation. For example,

  interface Monad m => MonadState s (m : Type -> Type) | m where
    get : m s
    pur : s -> m ()

  In the interface above, only `m` needs to be known in order to determine concrete implementations for `MonadState` without any dependency on `s`. Hence we mark
  `m` as the dependent parameter using `| m` in the interface definition.
-}
module Work

%default total

{-
  interface Show a where
    show : a -> String

  The interface above generates a method like below;

  show : Show a => a -> String
-}

data MyNat = Z | S MyNat 

add : MyNat -> MyNat -> MyNat
add Z m = m
add (S n) m = S (add n m)

partial
int2nat : Int -> MyNat
int2nat 0 = Z
int2nat n = S (int2nat (n - 1))

nat2int : MyNat -> Int
nat2int Z = 0
nat2int (S n) = 1 + nat2int n

-- define an instance of Show for MyNat

Show MyNat where
  show Z = "Z"
  show (S n) = "s" ++ show n

-- only one implementation can be present for an interface-type pair

data MyVect : Nat -> Type -> Type where
  Nil : MyVect Z a
  (::) : a -> MyVect k a -> MyVect (S k) a

Show a => Show (MyVect n a) where
  show xs = "{" ++ aux xs ++ "}" where
    aux : forall n .  MyVect n a -> String
    aux Nil = ""
    aux (x :: Nil) = show x
    aux (x :: xs) = show x ++ ", " ++ aux xs

{-
  interface Eq a where
    (==) : a -> a -> Bool
    (/=) : a -> a -> Bool

    x /= y = not (x == y)
    x == y = not (x /= y)


  data Ordering = LT | EQ | GT

  interface Eq a => Ord a where
    compare : a -> a -> Ordering

    (<) : a -> a -> Bool
    (<=) : a -> a -> Bool
    (>) : a -> a -> Bool
    (>=) : a -> a -> Bool
    max : a -> a -> a
    min : a -> a -> a
-}

Eq MyNat where
  Z == Z = True
  (S x) == (S y) = x == y
  _ == _ = False

{-
  interface Functor (f : Type -> Type) where
    map : (m : a -> b) -> f a -> f b

-}

data MyList a = Empty | Cons a (MyList a)

Show a => Show (MyList a) where
  show Empty = "[]"
  show (Cons x xs) = show x ++ "::" ++ show xs

Functor MyList where
  map f Empty = Empty
  map f (Cons x xs) = Cons (f x) (map f xs)

listToMyList : List Int -> MyList Int
listToMyList [] = Empty
listToMyList (x :: xs) = Cons x (listToMyList xs)

li : MyList Int
li = listToMyList [1..10]

{-
  infixl 2 <*>

  interface Functor f => Applicative (f : Type -> Type) where
    pure : a -> f a
    (<*>) : f (a -> b) -> f a -> f b


-}

append : MyList a -> MyList a -> MyList a
append Empty ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

Applicative MyList where
  -- pure : a -> MyList a
  pure x = Cons x Empty

  --(<*>) : MyList (a -> b) -> MyList a -> MyList b
  Empty <*> _ = Empty
  _ <*> Empty = Empty
  (Cons f fs) <*> xs = append (map f xs) (fs <*> xs)

{-
  interface Applicative m => Monad (m : Type -> Type) where
    (>>=) : m a -> (a -> m b) -> m b


    x <- v is the same as v >>= (\x => e)

    v; e is the same as v >>= (\_ => e)

    let x = v ; e is the same as let x = v in e

-}

Monad MyList where
  -- (>>=) : MyList a -> (a -> MyList b) -> MyList b
  Empty >>= _ = Empty
  (Cons x xs) >>= f = append (f x) (xs >>= f)

madd : Maybe Int -> Maybe Int -> Maybe Int
madd mx my = do x <- mx
                y <- my
                pure (x + y)

-- Idris can have different bind operators. `do` is simply syntactic sugar, so we can, as shown here for instance,
-- indicate that we mean to use the `do` (>>=) provided in the Prelude. We could very well have our own custom
-- implementations of the same.
--
-- Basically, there is no need for the bind (>>=) operator to be THE operator defined in the Monad interface.
msub : Maybe Int -> Maybe Int -> Maybe Int
msub mx my = Prelude.do x <- mx
                        y <- my
                        pure (x - y)
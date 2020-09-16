module DataTypes

data MyNat = Z | S MyNat

int2nat : Int -> MyNat
int2nat 0 = Z
int2nat n = S (int2nat (n - 1))

nat2int : MyNat -> Int
nat2int Z = 0
nat2int (S n) = 1 + nat2int n

plus : MyNat -> MyNat -> MyNat
plus Z m = m
plus (S n) m = S (plus n m)

mult : MyNat -> MyNat -> MyNat
mult Z m = Z
mult (S n) m = plus m (mult n m)

data MyList a = Nil | (::) a (MyList a)

infixr 10 ::

len : MyList a -> Nat
len Nil = Z
len (_ :: xs) = 1 + len xs


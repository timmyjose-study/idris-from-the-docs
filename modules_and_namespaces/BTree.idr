module BTree 

%default total

public export
data BTree : Type -> Type where
  Leaf : BTree a
  Node : BTree a -> a -> BTree a -> BTree a

export
insert : Ord a => a -> BTree a -> BTree a
insert x Leaf = Node Leaf x Leaf
insert x tree@(Node l v r) = case compare x v of
                              LT => Node (insert x l) v r
                              GT => Node l v (insert x r)
                              EQ => tree

export
toList : BTree a -> List a
toList Leaf = []
toList (Node l v r) = toList l ++ [v] ++ toList r

export
toTree : Ord a => List a -> BTree a
toTree [] = Leaf
toTree (x :: xs) = insert x (toTree xs)
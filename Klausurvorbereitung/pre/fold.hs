data N = Z | S N deriving Show
data Tree a = Leaf | Branch (Tree a) a (Tree a) deriving Show
data List a = Nil | Cons a (List a)

plus :: N -> N -> N
plus x y = case x of
  Z -> y
  S x' -> S (plus x' y)

foldTree leaf branch t = case t of
  Leaf -> leaf
  Branch l k r -> branch (foldTree leaf branch l) k (foldTree leaf branch r)

foldList nil cons l = case l of
  Nil -> nil
  Cons x xs -> cons x (foldList nil cons xs)

foldPeano z s v = case v of
  Z -> z
  S x -> s (foldPeano z s x)

summeTree = foldTree Z (\ x y z -> plus x (plus y z) )

summeList = foldList Z (\ x y -> plus x y)

dbl = foldPeano Z (\x -> S (S x))

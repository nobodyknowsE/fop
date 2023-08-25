import Prelude hiding (reverse)

data List a = Nil | Cons a (List a) deriving Show

append :: List a -> List a -> List a
append xs ys = case xs of
  Nil -> ys
  Cons x xs' -> Cons x (append xs' ys)

reverse :: List a -> List a
reverse xs = case xs of
  Nil -> Nil
  Cons x xs' -> append (reverse xs') (Cons x Nil)

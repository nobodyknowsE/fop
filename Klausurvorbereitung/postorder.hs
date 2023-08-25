import Prelude hiding (reverse)

data List a = Nil | Cons a (List a) deriving Show
data Bin a = Leaf | Branch (Bin a) a (Bin a) deriving Show

append :: List a -> List a -> List a
append xs ys = case xs of
  Nil -> ys
  Cons x xs' -> Cons x (append xs' ys)

reverse :: List a -> List a
reverse l = case l of
  Nil -> Nil
  Cons x xs' -> append (reverse xs') (Cons x Nil)

postorder :: Bin a -> List a
postorder t = case t of
  Leaf -> Nil
  Branch pr x po -> reverse ( Cons x ( append (reverse (postorder po) ) (reverse (postorder pr) ) ) ) 

data Tree a = Leaf | Branch (Tree a) a (Tree a) deriving Show

instance Functor Tree where
  fmap f t = case t of
    Leaf -> Leaf
    Branch l k r -> Branch (fmap f l) (f k) (fmap f r)

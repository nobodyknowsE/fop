data Tree a = Leaf a | Branch a (Tree a) (Tree a) deriving Show

foldT leaf branch t = case t of
  Leaf k -> leaf k
  Branch k l r -> branch k (foldT leaf branch l) (foldT leaf branch r)

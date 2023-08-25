data Tree a = Leaf | Branch (Tree a) a (Tree a)

fold leaf branch = \t -> case t of
  Leaf -> leaf
  Branch l k r -> branch (fold leaf branch l) k (fold leaf branch r)

summe = fold Z (\x y z -> plus x (plus y z))

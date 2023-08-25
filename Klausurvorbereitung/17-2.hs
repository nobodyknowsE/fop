module Blueprint where

    data Tree k = Leaf | Branch (Tree k) k (Tree k)
      deriving (Show, Eq)

    t1 :: Tree Bool
    t1 = Branch (Branch Leaf False Leaf) True (Branch (Branch Leaf True Leaf) False Leaf)

    t2 :: Tree Bool
    t2 = Branch (Branch (Branch Leaf False Leaf) True Leaf) True (Branch Leaf False Leaf)

    test :: Bool
    test = t1 /= t2 && inorder t1 == xs && inorder t2 == xs

    xs :: List Bool
    xs = Cons False (Cons True (Cons True (Cons False Nil)))

    data List k = Nil | Cons k (List k)
      deriving (Show, Eq)

    append :: List k -> List k -> List k
    append Nil ys = ys
    append (Cons x xs) ys = Cons x (append xs ys)

    inorder :: Tree k -> List k
    inorder t = case t of
      Leaf -> Nil
      Branch l k r -> append (inorder l) (Cons k (inorder r))

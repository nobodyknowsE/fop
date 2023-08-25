data Term
  = Var String | App Term Term | Abs String Term

data Dir = L | R | O
type Pos = List Dir

data List k = Nil | Cons k (List k)
      deriving (Show, Eq)

append :: List k -> List k -> List k
append Nil ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

pos :: Term -> List Pos -- alle Positionen
pos (Var s) = Nil
pos (App l r) = append
                    (append
                            (Cons L Nil) :: Pos
                            (append
                                    (Cons L Nil) :: Pos
                                    (pos l)
                            )
                    )
                    (append
                            (Cons R Nil) :: Pos
                            (append
                                    (Cons R Nil) :: Pos
                                    (pos r)
                            )
                    )
pos (Abs l r) = case r of
                  Var s -> append (Cons L Nil) :: Pos (Cons R Nil) :: Pos
                  -- App l r -> append (Cons L Nil) :: Pos (append (Cons R Nil) (pos r)) :: Pos
                  -- Abs l r -> append (Cons L Nil) :: Pos (append (Cons R Nil) (pos r)) :: Pos

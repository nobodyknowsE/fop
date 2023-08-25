data List a = Nil | Cons a (List a) deriving Show

append :: List a -> List a -> List a
append xs ys = case xs of
  Nil -> ys
  Cons x xs' -> Cons x (append xs' ys)

data Term = Var String | App Term Term
data Dir = L | R | O
type Pos = List Dir

pos :: Term -> List Pos
pos t = case t of
  Var x -> Cons (Cons O Nil) Nil
  App x y -> 

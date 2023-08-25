data N = Z | S N deriving Show

plus :: N -> N -> N
plus x y = case x of
  Z -> y
  S x' -> S (plus x' y)

times :: N -> N -> N
times x y = case x of
  Z -> Z
  S x' -> plus y (times x' y)

pow :: N -> N -> N
pow x y = case x of
  Z -> S Z
  S x' -> times y (pow x' y)

foldPeano z s n = case n of
  Z -> z
  S n' -> s (foldPeano z s n')

plusf x y = foldPeano y (\a -> S a) x

timesf x y = foldPeano Z (\a -> plusf a y) x

powf x y = foldPeano (S Z) (\a -> timesf y a) x

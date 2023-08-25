data N =  Z | S N deriving Show

foldN :: a -> (a -> a) -> N -> a
foldN z s n = case n of
  Z -> z
  S n' -> s (foldN z s n')

plus x y = foldN y (\a -> S a) x

mul x y = foldN Z (\a -> plus y a) x

pow x y = foldN (S Z) (\a -> mul x a) y

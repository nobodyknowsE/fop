data N = Z | S N deriving Show
f :: N -> N -> N
f Z y = y ; f (S x) y = S (f x y) ;

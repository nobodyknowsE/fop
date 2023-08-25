data N = Z | S N

plus :: N -> N -> N
plus Z y = y
plus (S x) y = S (plus x y)

f :: N -> N
f Z = Z
f (S x) = S (S (f x))

Lemma : forall x :: N, y :: N : f (plus x y) .=. plus (f x) (f y)
Proof by induction on x :: N
Case Z
  Show : f (plus Z y) .=. plus (f Z) (f y)
  Proof by rewriting f (plus Z y)
    (by def plus) .=. f y
    (by def plus) .=. plus Z (f y)
    (by def f) .=. plus (f Z) (f y)
  QED
Case S x'
  Fix x' :: N
  Assume IV :
    f (plus x' y) .=. plus (f x') (f y)
  Then Show:
    f (plus (S x') y) .=. plus (f (S x')) (f y)
  Proof by rewriting f (plus (S x') y)
    (by def plus) .=. f (S (plus x' y))
    (by def f) .=. S (S (f (plus x' y)))
    (by IV) .=. S (S (plus (f x') (f y)))
    (by def plus) .=. S (plus (S (f x')) (f y))
    (by def plus) .=. plus (S (S (f x'))) (f y)
    (by def f) .=. plus (f (S x')) (f y)
  QED
QED

data N = Z | S N 

plus :: N -> N -> N
plus x y = case x of
  Z -> y
  S a -> S (plus a y)

Lemma plus_assoc : forall a :: N, b :: N, c :: N : plus a (plus b c) .=. plus (plus a b) c
Proof by induction on a :: N
Case Z
  Show : plus Z (plus b c) .=. plus (plus Z b) c
  Proof by rewriting plus Z (plus b c)
    (by def plus) .=. plus b c
    (by def plus) .=. plus (plus Z b) c
  QED
Case S x
  Fix x :: N
  Assume IV : plus x (plus b c) .=. plus (plus x b) c
  Then Show: plus (S x) (plus b c) .=. plus (plus (S x) b) c
  Proof by rewriting plus (S x) (plus b c)
    (by def plus) .=. S (plus x (plus b c))
    (by IV) .=. S (plus (plus x b) c)
    (by def plus) .=. plus (S (plus x b)) c
    (by def plus) .=. plus (plus (S x) b) c
  QED
QED

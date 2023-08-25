data N = Z | S N

plus :: N -> N -> N
plus Z y = y
plus (S a) y = S (plus a y)

f :: N -> N
f Z = Z
f (S a) = S (S (f a))

Lemma func : forall x :: N, y :: N :
  f (plus x y) .=. plus (f x) (f y)
Proof by induction on x::N
Case Z
  Show : f (plus Z y) .=. plus (f Z) (f y)
  Proof by rewriting f (plus Z y)
    (by def plus) .=. f y
    (by def plus) .=. plus Z (f y)
    (by def f) .=. plus (f Z) (f y)
  QED
Case (S a)
  Fix a :: N
  Assume IV :
    f (plus a y) .=. plus (f a) (f y)
  Then Show :
    f (plus (S a) y) .=. plus (f(S a)) (f y)
  Proof by rewriting f (plus (S a) y)
    (by def plus) .=. f (S(plus a y))
    (by def f) .=. S (S (f (plus a y) ) )
    (by IV) .=. S (S (plus (f a) (f y)))
    (by def plus) .=. S( plus (S (f a)) (f y))
    (by def plus) .=. plus (S (S (f a))) (f y)
    (by def f) .=. plus (f (S a)) (f y)
  QED
QED

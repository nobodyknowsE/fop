data N = Z | S N deriving Show

plus1 :: N -> N -> N
plus1 x y = case x of
  Z -> y
  S a -> S(plus1 a y)

plus2 :: N -> N -> N
plus2 x y = case x of
  Z -> y
  S a -> plus2 a (S y)

Lemma hilfssatz : forall x :: N, y :: N : plus1 x (S y) .=. plus1 (S x) y
Proof by induction on x :: N
Case Z
  Show : plus1 Z (S y) .=. plus1 (S Z) y
  Proof by rewriting plus1 Z (S y)
    (by def plus1) .=. S y
    (by def plus1) .=. S (plus1 Z y)
    (by def plus1) .=. plus1 (S Z) y
  QED
Case S x'
  Fix x' :: N
  Assume IV :
    plus1 x' (S y) .=. plus1 (S x') y
  Then Show :
    plus1 (S x') (S y) .=. plus1 S (S x') y
  Proof by rewriting plus1 (S x') (S y)
    (by def plus1) .=. S (plus1 x' (S y))
    (by IV) .=. S (plus1 (S x') y)
    (by def plus1) .=. plus1 S (S x') y
  QED

Lemma plus_alt : forall x :: N, y :: N : plus2 x y .=. plus1 x y
Proof by induction on x :: N generalizing y :: N
Case Z
  Show : plus2 Z y .=. plus1 Z y
  Proof by rewriting plus2 Z y
    (by def plus2) .=. y
    (by def plus1) .=. plus1 Z y
  QED
Case S x'
  Fix x' :: N
  Assume IV :
    plus2 x' y .=. plus1 x' y
  Then Show :
    plus2 (S x') y .=. plus1 (S x') y
  Proof by rewriting plus2 (S x') y
    (by def plus2) .=. plus2 x' (S y)
    (by IV) .=. plus1 x' (S y)
    (by hilfssatz) .=. plus1 (S x') y
  QED
QED

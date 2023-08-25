Lemma HS : forall x :: N, y :: N :
  plus x (S y) .=. plus (S x) y
Proof by induction on x :: N
-- IA
Case Z
  Show : plus Z (S y) .=. plus (S Z) y
  Proof by rewriting plus Z (S y)
    (by def plus) .=. S y
    () .=. 1 + y -- ???
    () .=. (S Z) + y -- ???
    () .=. plus  (S Z) y -- ???
  QED
-- IS
Case (S x2)
  Fix x2 :: N
  -- IV
  Assume IV:
    plus x2 (S y) = plus (S x2) y
  -- IB
  Then Show:
    plus (S x2) (S y) .=. plus (S(S x2)) y
  Proof by rewriting plus (S x2) (S y)
    (by def plus) .=. S(plus x2 (S y))
    (by IV) .=. S(plus (S x2) y)
    (by def plus) .=. plus (S (S x2)) y
  QED



Lemma plus_alt: forall x :: N, y :: N :
  plus_ x y .=. plus x y
Proof by induction on x :: N generalizing y :: N
-- IA
Case Z
  Show : plus_ Z y .= . plus Z y
  Proof by rewriting plus_ Z y
    (by def plus_) .=. y
    (by def plus) .=. plus Z y
-- IS
Case (S x2)
  Fix x2 :: N
  -- IV
  Assume IV:
    plus_ x2 y .=. plus x2 y
  -- IB
  Then Show:
    plus_ (S x2) y .=. plus (S x2) y
  Proof by rewriting plus_ (S x2) y
    (by def plus_) .=. plus_ x2 (S y)
    (by IV) .=. plus x2 (S y)
    (by HS) .=. plus (S x2) y
  QED

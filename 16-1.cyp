data N = Z | S N

doubleN :: N -> N
doubleN Z = Z
doubleN (S x) = S (S (doubleN x))


data B = Zero | Even B | Odd B

value :: B -> N
value Zero = Z
value (Even x) = doubleN (value x)
value (Odd x) = S (doubleN (value x))


succB :: B -> B

succB Zero = Odd Zero
succB (Even x) = Odd x
succB (Odd x) = Even (succB x)


Lemma succ : forall x :: B :  S (value x) .=. value (succB x)
Proof by induction on x :: B
Case Zero
  Show : S (value Zero) .=. value (succB Zero)
  Proof by rewriting S (value Zero)
    (by def value) .=. S (Z)
    (by def doubleN) .=. S (doubleN Z)
    (by def value) .=. S (doubleN (value Zero))
    (by def value) .=. value (Odd Zero)
    (by def succB) .=. value (succB Zero)
  QED
Case Even b
  Fix b::B
  Assume IV:
    S (value b) .=. value (succB b)
  Then Show:
    S (value (Even b)) .=. value (succB (Even b))
  Proof by rewriting S (value (Even b))
    (by def value) .=. S (doubleN (value b))
    (by def value) .=. value (Odd b)
    (by def succB) .=. value (succB (Even b))
  QED
Case Odd b
  Fix b::B
  Assume IV:
    S (value b) .=. value (succB b)
  Then Show:
    S (value (Odd b)) .=. value (succB (Odd b))
  Proof by rewriting S (value (Odd b))
    (by def value) .=. S (S (doubleN (value b)))
    (by def doubleN) .=. doubleN (S (value b))
    (by IV) .=. doubleN (value (succB b))
    (by def value) .=.  value (Even (succB b))
    (by def succB) .=. value (succB (Odd b))
  QED
QED

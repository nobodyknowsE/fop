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
      (by def value) .=. S Z
      (by def doubleN) .=. S (doubleN Z)
      (by def value) .=. S (doubleN (value Zero))
      (by def value) .=. value (Odd Zero)
      (by def succB) .=. value (succB Zero)
    QED
Case Even x'
  Fix x' :: B
  Assume IV :
    S (value x') .=. value (succB x')
  Then Show :
    S (value (Even x')) .=. value (succB (Even x'))
  Proof by rewriting S (value (Even x'))
    (by def value) .=. S (doubleN (value x'))
    (by def value) .=. value (Odd x')
    (by def succB) .=. value (succB (Even x'))
  QED
Case Odd x'
  Fix x' :: B
  Assume IV :
    S (value x') .=. value (succB x')
  Then Show :
    S (value (Odd x')) .=. value (succB (Odd x'))
  Proof by rewriting S (value (Odd x'))
    (by def value) .=. S (S (doubleN (value x')))
    (by def doubleN) .=. doubleN (S (value x'))
    (by IV) .=. doubleN (value (succB x'))
    (by def value) .=. value (Even (succB x'))
    (by def succB) .=. value (succB (Odd x'))
  QED
QED

data Bool = False | True

not :: Bool -> Bool
not False = True
not True = False

Lemma nnx: forall x :: Bool : not (not x) .=. x
Proof by case analysis on x :: Bool
  Case False
    Assume XF: x .=. False
    Then Proof by rewriting not (not x)
    (by XF) .=. not (not False)
    (by def not) .=. not True
    (by def not) .=. False
  QED
  Case True
    Assume XT : x .=. True
    Then Proof by rewriting not (not x)
    (by XT) .=. not (not True)
    (by def not) .=. not False
    (by def not) .=. True
  QED
QED

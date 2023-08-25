data Bool = False | True

not :: Bool -> Bool
not False = True
not True = False

Lemma nnf: not (not False) .=. False
Proof by rewriting not (not False)
  (by def not) .=. not True
  (by def not) .=. False
QED

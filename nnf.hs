Lemma nnf: not (not False) .=. False
Proof by rewriting not(not False)
  (by def not) .=. not True
  (by def not) .=. False
QED

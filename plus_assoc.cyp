Lemma plus_assoc : forall a :: N, b :: N, c :: N :
plus a (plus b c) .=. plus (plus a b) c
Proof by induction on a :: N
Case Z
Show : plus Z (plus b c) .=. plus (plus Z b) c
Proof ... QED
Case S x
Fix x :: N
Assume IV :
plus x (plus b c).=. plus (plus x b) c
Then Show :
plus(S x)(plus b c) .=. plus (plus (S x) b) c
Proof ... QED
QED

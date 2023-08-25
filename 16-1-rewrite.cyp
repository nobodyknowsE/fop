a :: T -> T
b :: T -> T

axiom E : forall x :: T : a(b(a(x))) .=. x

Lemma : forall x :: T : a(b(b(b(a(a(x)))))) .=. b(b(a(x)))
Proof by rewriting         a(b(b(b(a(a(x))))))
    (by E) .=. a(b(a(b(a(b(b(a(a(x)))))))))
    (by E) .=. b(a(b(b(a(a(x))))))
    (by E) .=. b(a(b(a(b(a(b(a(a(x)))))))))
    (by E) .=. b(b(a(b(a(a(x))))))
    (by E)   .=. b(b(a(x)))
QED

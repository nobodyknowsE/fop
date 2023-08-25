data List a = Nil | Cons a (List a)

append :: List a -> List a -> List a
append Nil ys = ys
append ( Cons x xss ) ys = Cons x (append xss ys)

reverse :: List a -> List a
reverse Nil = Nil
reverse (Cons x xs) = append (reverse xs) (Cons x Nil)

rev_app :: List a -> List a -> List a
rev_app Nil ys = ys
rev_app (Cons x xs) ys = rev_app xs (Cons x ys)

Lemma append_assoc : forall xs :: List a, ys :: List a , zs :: List a :
  append xs (append ys zs) .=. append (append xs ys) zs
Proof by induction on xs :: List a
  Case Nil
    Show:  append Nil (append ys zs) .=. append (append Nil ys) zs
    Proof by rewriting append Nil (append ys zs)
    (by def append) .=. append ys zs
    (by def append) .=. append (append Nil ys) zs
    QED
  Case Cons x xs'
    Fix x :: a, xs' :: List a
    Assume IV:
      append xs' (append ys zs).=. append (append xs' ys) zs
    Then Show:
      append (Cons x xs') (append ys zs).=. append (append (Cons x xs') ys) zs
    Proof by rewriting append (Cons x xs') (append ys zs)
      (by def append) .=. Cons x (append xs' (append ys zs))
      (by IV) .=. Cons x (append (append xs' ys) zs)
      (by def append) .=. append (Cons x (append xs' ys)) zs
      (by def append) .=. append (append (Cons x xs') ys) zs
    QED
QED

Lemma : forall xs :: List a, ys :: List a :
  rev_app xs ys .=. append (reverse xs) ys
Proof by induction on xs :: List a generalizing ys :: List a
 Case Nil
   For fixed ys :: List a
   Show: rev_app Nil ys .=. append (reverse Nil) ys
   Proof by rewriting rev_app Nil ys
    (by def rev_app) .=. ys
    (by def append) .=. append Nil ys
    (by def reverse) .=. append (reverse Nil) ys
    QED
 Case Cons x xs'
   Fix x :: a,  xs' :: List a
   Assume IV:
     forall ys :: List a : rev_app xs' ys .=. append (reverse xs') ys
   Then For fixed ys :: List a
     Show :
       rev_app (Cons x xs') ys .=. append (reverse (Cons x xs')) ys
   Proof by rewriting rev_app (Cons x xs') ys
     (by def rev_app) .=. rev_app xs' (Cons x ys)
     (by IV) .=. append (reverse xs') (Cons x ys)
     (by def append) .=. append (reverse xs') (Cons x (append Nil ys))
     (by def append) .=. append (reverse xs') (append (Cons x Nil) ys)
     (by append_assoc) .=. append (append (reverse xs') (Cons x Nil)) ys
     (by def reverse) .=. append (reverse (Cons x xs')) ys
   QED
QED

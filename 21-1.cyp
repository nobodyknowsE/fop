data List e = Nil | Cons e (List e)

append :: List e -> List e -> List e
append Nil ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

data Bool = False | True

bool :: Bool -> a -> a -> a
bool False x y = x
bool True  x y = y

takeWhile :: (e -> Bool) -> List e -> List e
takeWhile p Nil = Nil
takeWhile p (Cons x xs) = bool (p x) Nil (Cons x (takeWhile p xs))

dropWhile :: (e -> Bool) -> List e -> List e
dropWhile p Nil = Nil
dropWhile p (Cons x xs) = bool (p x) (Cons x xs) (dropWhile p xs)

Lemma : forall  p :: e -> Bool, xs :: List e
  : append (takeWhile p xs) (dropWhile p xs) .=. xs
Proof by induction on xs :: List e
Case Nil
  Show : append (takeWhile p Nil) (dropWhile p Nil) .=. Nil
  Proof by rewriting append (takeWhile p Nil) (dropWhile p Nil)
    (by def takeWhile) .=. append Nil (dropWhile p Nil)
    (by def append) .=. dropWhile p Nil
    (by def dropWhile) .=. Nil
  QED
Case Cons x xs'
  Fix x :: e, xs' :: List e
  Assume
    IH: append (takeWhile p xs') (dropWhile p xs') .=. xs'
  Then
    Show: append (takeWhile p (Cons x xs')) (dropWhile p (Cons x xs')) .=. Cons x xs'
  Proof by case analysis on p x :: Bool
    Case False
      Assume A : p x .=. False
      Then Proof by rewriting append (takeWhile p (Cons x xs')) (dropWhile p (Cons x xs'))
        (by def takeWhile) .=. append (bool (p x) Nil (Cons x (takeWhile p xs'))) (dropWhile p (Cons x xs'))
        (by A) .=. append (bool False Nil (Cons x (takeWhile p xs'))) (dropWhile p (Cons x xs'))
        (by def bool) .=. append Nil (dropWhile p (Cons x xs'))
        (by def append) .=. dropWhile p (Cons x xs')
        (by def dropWhile) .=. bool (p x) (Cons x xs') (dropWhile p xs')
        (by A) .=. bool False (Cons x xs') (dropWhile p xs')
        (by def bool) .=. Cons x xs'
      QED
      Case True
        Assume B : p x .=. True
        Then Proof by rewriting append (takeWhile p (Cons x xs')) (dropWhile p (Cons x xs'))
          (by def takeWhile) .=. append (bool (p x) Nil (Cons x (takeWhile p xs'))) (dropWhile p (Cons x xs'))
          (by B) .=. append (bool True Nil (Cons x (takeWhile p xs'))) (dropWhile p (Cons x xs'))
          (by def bool) .=. append (Cons x (takeWhile p xs')) (dropWhile p (Cons x xs'))
          (by def dropWhile) .=. append (Cons x (takeWhile p xs')) (bool (p x) (Cons x xs') (dropWhile p xs'))
          (by B) .=. append (Cons x (takeWhile p xs')) (bool True (Cons x xs') (dropWhile p xs'))
          (by def bool) .=. append (Cons x (takeWhile p xs')) (dropWhile p xs')
          (by def append) .=. Cons x (append (takeWhile p xs') (dropWhile p xs'))
          (by IH) .=. Cons x xs'
        QED
  QED
QED

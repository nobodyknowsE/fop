import Prelude hiding (and,length,not)

data List a = Nil | Cons a (List a) deriving Show
data N = Z | S N deriving Show
data Pair a b = Pair a b deriving Show

fold :: b -> (a -> b -> b) -> List a -> b
fold nil cons xs = case xs of
  Nil -> nil
  Cons x xs' -> cons x (fold nil cons xs')

and = fold True (&&)
length = fold Z (\x y -> S y)

foldB false true v = case v of
  False -> false
  True -> true

not = foldB True False

-- foldMaybe :: b -> (a -> b) -> Maybe a -> b
foldMaybe nothing just a = case a of
  Nothing -> nothing
  Just a' -> just a'

-- foldPair :: (t1 -> t2 -> t3) -> Pair t1 t2 -> t3
foldPair pair t = case t of
  Pair a b -> pair a b

-- foldEither :: (a -> b) -> (c -> b) -> Either a c -> b
foldEither left right v = case v of
  Left a -> left a
  Right b -> right b

swap = foldEither (\x -> Right x) (\x -> Left x)

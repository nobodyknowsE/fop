data Pair a b = Pair a b deriving Show

foldPair :: (a -> b -> c) -> (Pair a b) -> c
foldPair pair n = case n of
  Pair a b -> pair a b

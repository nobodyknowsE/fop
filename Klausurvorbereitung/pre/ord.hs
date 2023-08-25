data T = T Int Int deriving Show

instance Eq T where
  T x y == T a b = (x == a) && (y == b)

instance Ord T where
  T x y <= T a b = case (x <= a) of
    False -> False
    True -> True

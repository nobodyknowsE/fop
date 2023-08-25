import Data.Ord
import Data.Semigroup

data Pair a b = Pair a b deriving (Eq,Show)

instance (Ord a, Ord b) => Ord (Pair a b) where
  compare (Pair px py) (Pair qx qy) = case (px < qx) of
    False -> case (px > qx) of
      False -> case (py < qy) of
        False -> case (py > qy) of
          False -> EQ
          True -> GT
        True -> LT
      True -> GT
    True -> LT

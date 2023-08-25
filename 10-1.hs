import Test.LeanCheck
-- enable type applications with :set -XTypeApplications

data T = T Bool Bool Bool deriving Show

-- Wörterbuch erzeugen
instance Eq T where -- komponentenweise Gleicheit testen
  (T x1 y1 z1) == (T x2 y2 z2) = (x1 == x2) && (y1 == y2) && (z1 == z2)

-- Wörterbuch erzeugen
instance Ord T where -- komponentenweise auf kleiner testen
  (T x1 y1 z1) < (T x2 y2 z2) = case (x1 < x2) of -- kleiner-Relation
    True -> True
    False -> case (x1 > x2) of
      True -> False
      False -> case (y1 < y2) of
        True -> True
        False -> case (y1 > y2) of
          True -> False
          False -> (z1 < z2)
  l <= r = (l < r) || (l == r) -- kleiner-gleich-Relaltion

instance Listable T where tiers = cons3 T -- notwendig fürs Testen

leq_is_transitive :: Ord a => a -> a -> a -> Bool
leq_is_transitive = \ x y z -> case ((x <= y) && (y <= z)) of -- ((x <= y) && (y <= z)) = A, (x <= z) = B, A -> B
  False -> True -- "aus falschem folgt beliebiges"
  True -> (x <= z)

leq_is_antisymmetric :: Ord a => a -> a -> Bool
leq_is_antisymmetric = \ x y -> case ((x <= y) && (y <= x)) of -- ((x <= y) && (y <= x)) = A, (x == y) = B, A -> B
  False -> True
  True -> (x == y)

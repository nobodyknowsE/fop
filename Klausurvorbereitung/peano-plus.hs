data N = Z | S N deriving (Show, Eq)

plus :: N -> N -> N
plus x y = case x of
  Z -> y
  S a -> S (plus a y)

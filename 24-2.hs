{-# language TypeApplications #-}

    module Blueprint where

    import Test.LeanCheck

    data T = T Bool Bool Bool
      deriving (Eq, Show)

    instance Listable T where tiers = cons3 T

    instance Semigroup T where -- Semigruppe besteht aus Menge und zweistelliger, associativer Verknüpfung (<>)
      T a b c <> T x y z = case (a || b) || c of
        True -> T a b c
        False -> T x y z

    associative :: (Eq a, Semigroup a) => a -> a -> a -> Bool
    associative = \ p q r -> (p <> q) <> r == p <> (q <> r)

    instance Monoid T where -- Monoid ist Halbgruppe mit neutralem Element
      mempty = T False False False

    neutral_left :: (Eq a, Monoid a) => a -> Bool
    neutral_left = \ p -> mempty <> p == p

    neutral_right :: (Eq a, Monoid a) => a -> Bool
    neutral_right = \ p -> p <> mempty == p

    commutative :: (Eq a, Semigroup a) => a -> a -> Bool
    commutative = \ p q -> p <> q == q <> p

    test = and
      [ holds 100 $ associative @T
      , holds 100 $ neutral_left @T
      , holds 100 $ neutral_right @T
      , not $ holds 100 $ commutative @T
      ]

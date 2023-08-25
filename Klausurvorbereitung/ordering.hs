{-# language TypeApplications #-}

    module Blueprint where

    import Test.LeanCheck
    import Data.Ord
    import Data.Semigroup
    import Data.Monoid

    associative :: (Eq a, Semigroup a) => a -> a -> a -> Bool
    associative = \ p q r -> (p <> q) <> r == p <> (q <> r)

    neutral_left :: (Eq a, Monoid a) => a -> Bool
    neutral_left = \ p -> mempty <> p == p

    neutral_right :: (Eq a, Monoid a) => a -> Bool
    neutral_right = \ p -> p <> mempty == p

    commutative :: (Eq a, Semigroup a) => a -> a -> Bool
    commutative = \ p q -> p <> q == q <> p

    test = and
      [ holds 100 $ associative @Ordering
      , holds 100 $ neutral_left @Ordering
      , holds 100 $ neutral_right @Ordering
      , not $ holds 100 $ commutative @Ordering
      ]

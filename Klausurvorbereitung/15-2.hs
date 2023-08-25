{-# language DeriveGeneric #-}

    module Blueprint where

    import Test.LeanCheck
    import Test.LeanCheck.Generic
    import GHC.Generics

    data N = Z | S N deriving ( Eq, Show, Generic )

    instance Listable N where tiers = genericTiers

    half :: N -> N
    half x = case x of
      Z -> Z
      S Z -> S Z
      S (S Z) -> S Z
      S (S c) -> S(half c)

    spec :: N -> Bool
    spec = \ x -> half (plus x x) == x

    plus :: N -> N -> N
    plus x y = case x of
      Z -> y
      S x' -> S (plus x' y)

    test = null (counterExamples 100 spec)

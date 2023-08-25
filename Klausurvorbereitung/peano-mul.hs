{-# language DeriveGeneric #-}

module Blueprint where

import Test.LeanCheck
import Test.LeanCheck.Generic
import GHC.Generics

data N = Z | S N deriving ( Eq, Show, Generic )

instance Listable N where tiers = genericTiers

plus :: N -> N -> N
plus x y = case x of
  Z -> y
  S x' -> S (plus x' y)

times :: N -> N -> N
times x y = case x of
  Z -> Z
  S x' -> plus y (times x' y)

pow :: N -> N -> N
pow x y = case y of
  Z -> S Z
  S x' -> times x (pow x x')

comm = \ x y ->
  plus x y == plus y x

t0 = Z
t1 = S Z
t2 = S (S Z)
t3 = S (S (S Z))
t4 = S (S (S (S Z)))

test = null (counterExamples 1000 comm)

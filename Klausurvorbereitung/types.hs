{-# language ExplicitForAll, KindSignatures, TypeApplications #-}

import Data.Kind (Type)

data List a = Nil | Cons a (List a) deriving Show
data N = Z | S N deriving Show

g :: forall (t :: Type) . t -> t
g x = x

f :: t -> t
f x = x

compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

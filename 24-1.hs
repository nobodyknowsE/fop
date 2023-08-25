{-# language DeriveGeneric #-}

    module Blueprint where

    import Test.LeanCheck
    import Test.LeanCheck.Generic
    import GHC.Generics

    import Prelude hiding (all)

    data Tree k = Leaf | Branch (Tree k) k (Tree k)
      deriving (Eq, Show, Generic)
    instance Listable k => Listable (Tree k) where tiers = genericTiers

    fold :: r -> (r -> k -> r -> r) -> Tree k -> r
    fold leaf branch t = case t of
      Leaf -> leaf
      Branch l k r -> branch (fold leaf branch l) k (fold leaf branch r)

    all :: (k -> Bool) -> Tree k -> Bool
    all p = fold True (\ l k r -> l && p k && r)

    inorder :: Tree k -> [k]
    inorder = fold [] ( \ x y z -> x ++ y : z )

    -- Boolesche Operatoren: && , || , not
    -- Vergleiche:  < , <=

    monotone :: Ord e => [e] -> Bool
    monotone xs = case xs of
      [] -> True
      x : ys -> case ys of
        [] -> True
        y : zs -> x <= y && monotone (y : zs)

    is_search_tree :: Ord k => Tree k -> Bool
    is_search_tree = fst . with_bounds

    prop1 :: Tree Integer -> Bool
    prop1 = \ t -> case with_bounds t of
      (ok, mb) -> ok ==> case mb of
        Nothing ->
          t == Leaf
        Just (lo,hi) ->
          t /= Leaf && all (\ e -> lo <= e && e <= hi) t

    prop2 :: Tree Integer -> Bool
    prop2 = \ t -> monotone (inorder t) == is_search_tree t

    with_bounds ::  Ord k => Tree k -> (Bool, Maybe (k, k))
    with_bounds = fold (True,Nothing)
      ( \ (okx, mbx) k (oky, mby) ->
          case mbx of
            Nothing -> case mby of
              Nothing -> (True, Just (k, k))
              Just (ylo, yhi) -> ((okx && oky)&&(k <= ylo), Just (k, yhi))
            Just (xlo, xhi) -> case mby of
              Nothing -> ((okx && oky)&&(xhi <= k), Just(xlo, k))
              Just (ylo, yhi) -> ((okx && oky)&&(xhi <= k)&&(k <= yhi), Just(xlo, yhi))
          )

    test = and [ with_bounds (Branch (Branch Leaf 0 Leaf) 5 (Branch Leaf 8 Leaf))
                     == (True, Just (0, 8))
               , is_search_tree
                 $ Branch (Branch Leaf 0 Leaf) 5 (Branch Leaf 8 Leaf)
               , not $ is_search_tree
                 $ Branch (Branch Leaf 0 Leaf) 5 (Branch (Branch Leaf 9 Leaf) 8 Leaf)
               , holds 200 prop1
               , holds 200 prop2
               ]

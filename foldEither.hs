foldEither :: (a -> c) -> (b -> c) -> (Either a b) -> c
foldEither left right n = case n of
  Left a -> left a
  Right b -> right b

foldMaybe :: b -> (a -> b) -> Maybe a -> b
foldMaybe nothing just n = case n of
  Nothing -> nothing
  Just a -> just a

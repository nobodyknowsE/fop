foldBool :: b -> b -> Bool -> b
foldBool false true n = case n of
  False -> false
  True -> true


-- inklusive OR f x y = foldBool y x x
-- AND          f x y = foldBool x y x

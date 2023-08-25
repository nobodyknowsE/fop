maj :: Bool -> Bool -> Bool -> Bool
maj x y z = case x of
  False -> case y of
    False -> False
    True -> case z of
      False -> False
      True -> True
  True -> case z of
    False -> case y of
      False -> False
      True -> True
    True -> True

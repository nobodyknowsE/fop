f x y z = case y || x of
  False -> y
  True -> case z && y of
    False -> z
    True -> False

g x y z = case y of -- strikt im zweiten Argument
  False -> x && z -- strikt im ersten Argument, falls y = False, nicht strikt im dritten Argument, falls x = False
  True -> case x /= z of -- strikt im ersten und dritten Argument, falls y = True
    False -> True
    True -> False

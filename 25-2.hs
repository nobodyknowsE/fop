{ f y z = g (g z True) (g y z); g y z = case z of {False -> True ; True -> y}}

guard :: Bool -> [()]
guard b = case b of False->[]; True->[()]

retur :: a -> [a] ; retur x = [x]

t = do x <- [1 .. 10]
       y <- [x .. 10]
       z <- [y .. 10]
       guard $ x**2 + y**2 == z**2
       retur (x,y,z)

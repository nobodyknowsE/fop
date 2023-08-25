data N = Z | S N

doubleN :: N -> N
doubleN Z = Z
doubleN (S x) = S (S (doubleN x))


data B = Zero | Even B | Odd B deriving Show

value :: B -> N
value Zero = Z
value (Even x) = doubleN (value x)
value (Odd x) = S (doubleN (value x))


succB :: B -> B

succB Zero = Odd Zero
succB (Even x) = Odd x
succB (Odd x) = Even (succB x)

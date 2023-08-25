data Term = E | F Term Term | G Term Term Term deriving Show

t1 = E
t2 = F E E
t3 = G (F E E) E E
t4 = G E E E :: Term

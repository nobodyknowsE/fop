data Tree = Leaf | Branch Tree Tree deriving Show

ungerade :: Tree -> Bool
ungerade x = case x of
          Leaf -> True
          Branch l r -> ungerade l /= ungerade r

-- Beispiele in ghci --
-- a = Leaf
-- b = Branch ( Branch Leaf Leaf ) Leaf
-- ungerade a
-- ungerade b

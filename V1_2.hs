-- mit anonymen Komponenten
data T = E | F T T | G T T T deriving Show

E :: T
G (F E E) (F E E) (E) :: T
F ( F E E ) E :: T
F E ( F E E ) :: T

-- mit benannten Komponenten
data T2 = E {}
        | F { left :: T2, right :: T2 }
        | G { left :: T2, middle :: T2, right :: T2}
        deriving Show

E {} :: T2
G { left: F { left: E {}, right: E {} }, middle: F { left: E {}, right: E {} }, right: E {} } :: T2

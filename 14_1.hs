module Blueprint where

import qualified Data.Set as S

-- aus Prelude importiert:
-- data Bool = False | True

data C = R | G | B deriving (Eq, Ord, Show)

data T = X C | Y Bool Bool deriving (Eq, Ord, Show)

solution :: S.Set T
solution = S.fromList [ X R, X G, X B, Y False False, Y False True, Y True True, Y True False ]

test :: Bool
test = S.size solution == 7

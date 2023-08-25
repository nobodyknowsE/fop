import Prelude hiding (lookup)

-- fromList in Data.Map: nur Schlüssel k hat Typconstraints Ord und Num,
-- weil die Schlüssel der Map eine totale Ordnung ergeben müssen, damit man in ihnen suchen kann
-- Werttyp a hingegen hat keinen Typconstraint, weil in der Map alle beliebigen Typen (nicht gleichzeitig) gespeichert werden können

-- singleton: wird in fromList mit einem Element übersetzt -> übernimmt Typconstraint Num für Schlüssel k
-- hat aber kein Typconstraint Ord für k, weil nur ein Element existiert und deshalb keine Schlüssel geordnet werden müssen

-- fromAscList: Typconstraints Ord k, Num k kommen von fromList

-- fromDistinctAscList: Typconstraints liegen in Definition - Liste mit aufsteigenden (also Ord und Num) Schlüsseln erwartet

data K = K Int deriving (Show,Eq)

instance Ord K where
  K a <= K b = rem (b+1) a == 0

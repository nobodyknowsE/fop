{ f p y x z =
    g (case (g x y) of
      { False -> z; True -> g True True }) x
; g x q =
    case (case x of
      { False -> case q of { False -> True; True -> x }; True -> True }) of
      { False -> True; True -> q }
}

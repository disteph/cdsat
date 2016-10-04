(set-logic empty)
(set-info :status unsat)
(declare-fun p () Bool)
(declare-fun q () Bool)
(assert (not (or (=> p q) (=> q p))))
(check-sat)
(exit)
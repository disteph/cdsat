(set-logic empty)
(set-info :status unsat)
(declare-fun p () Bool)
(assert (not (or p (not (not (not p))))))
(check-sat)
(exit)
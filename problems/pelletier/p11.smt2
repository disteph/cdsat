(set-logic empty)
(set-info :status unsat)
(declare-fun p () Bool)
(assert (not (<=> p p)))
(check-sat)
(exit)
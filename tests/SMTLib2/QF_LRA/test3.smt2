(set-logic QF_LRA)
(set-info :smt-lib-version 2.0)
(set-info :status unsat)
(declare-fun x () Real)
(declare-fun y () Real)
(declare-fun z () Real)
(assert (= x 5))
(assert (not (= x (+ 2 3))))
(check-sat)
(exit)

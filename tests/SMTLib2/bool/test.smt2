(set-logic QF_BOOL)
(set-info :smt-lib-version 2.0)
(set-info :status sat)
(declare-fun a () Bool)
(assert (= a (and a a)))
(exit)

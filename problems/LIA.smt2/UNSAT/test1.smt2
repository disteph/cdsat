(set-logic QF_LIA)
(set-info :source |
Alberto Griggio

|)
(set-info :smt-lib-version 2.0)
(set-info :category "random")
(set-info :status unsat)
(declare-fun x0 () Int)
(assert (and (<= (+ (* 1 x0)) (- 57))(> (+ (* 1 x0)) (- 50))))
(check-sat)
(exit)

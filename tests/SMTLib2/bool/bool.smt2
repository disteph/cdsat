(set-logic QF_BOOL)
(set-info :status unsat)
(declare-sort A 0)
(declare-const x A)
(declare-const y A)
(declare-const z A)
(declare-const a Bool)
(declare-const b Bool)
(declare-const c Bool)
(assert (or (not a) b))
(assert (or (not b) (not a)))
(assert (or a (not b)))
(assert (or b a))
(check-sat)

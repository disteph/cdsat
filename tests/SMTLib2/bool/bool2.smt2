(set-logic QF_BOOL)
(declare-sort A 0)
(declare-const x A)
(declare-const y A)
(declare-const z A)
(declare-const a Bool)
(declare-const b Bool)
(declare-const c Bool)
(assert (not (or a b)))
(check-sat)
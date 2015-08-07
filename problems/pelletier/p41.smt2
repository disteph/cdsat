(set-logic FO)
(set-info :status unsat)
(declare-sort Term 0)
(declare-fun f (Term Term) Bool)
(assert (forall ((z Term)) (exists ((y Term)) (forall ((x Term)) (<=> (f x y) (and (f x z) (not (f x x))))))))
(assert (exists ((z Term)) (forall ((x term)) (f x z))))
(check-sat)
(exit)

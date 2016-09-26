(set-logic FO)
(set-info :status unsat)
(declare-sort Term 0)
(declare-fun f (Term) Bool)
(declare-fun g (Term) Bool)
(declare-fun h (Term Term) Bool)
(declare-fun j (Term) Bool)
(assert (forall ((x Term)) (=> (f x) (and (exists ((y Term)) (and (g y) (h x y))) (exists ((y Term)) (and (g y) (not (h x y))))))))
(assert (exists ((x Term)) (and (j x) (forall ((y Term)) (=> (g y) (h x y))))))
(assert (not (exists ((x Term)) (and (j x) (not (f x))))))
(check-sat)
(exit)

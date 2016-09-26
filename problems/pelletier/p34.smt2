(set-logic FO)
(declare-sort Term 0)
(declare-fun p (Term) Bool)
(declare-fun q (Term) Bool)
(set-info :status unsat)
(assert (not (<=> 
    (<=>
        (exists ((x Term)) (forall ((y Term)) (<=> (p x) (p y))))
        (<=> (exists ((x Term)) (q x)) (forall ((y Term)) (q y)))
    )
    (<=>
        (exists ((x Term)) (forall ((y Term)) (<=> (q x) (q y))))
        (<=> (exists ((x Term)) (p x)) (forall ((y Term)) (p y)))
    )
)))
(check-sat)
(exit)

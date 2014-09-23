(set-logic FO)
(set-info :status unsat)
(declare-fun p (Term) Bool)
(declare-fun r (Term Term) Bool)
(declare-fun a () Term)
(assert (not (<=>
    (forall ((x Term)) (=>
        (and (p a) (=> (p x) (exists ((y Term)) (and (p y) (r x y)))))
        (exists ((z Term) (w Term)) (and (p z) (and (r x w) (r w z))))
    ))
    (forall ((x Term)) (and
        (or (not (p a)) (or (p x) (exists ((z Term) (w Term)) (and (p z) (and (r x w) (r w z))))))
        (or (not (p a)) (or (not (exists ((y Term)) (and (p y) (r x y)))) (exists ((z Term) (w Term)) (and (p z) (and (r x w) (r w z))))))
    ))
)))
(check-sat)
(exit)

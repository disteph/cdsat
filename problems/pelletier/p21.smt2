(set-logic FO)
(declare-fun p () Bool)
(declare-fun f (Term) Bool)
(set-info :status unsat)
(assert (exists ((x Term)) (=> p (f x))))
(assert (exists ((x Term)) (=> (f x) p)))
(assert (not (exists ((x Term)) (<=> p (f x)))))
(check-sat)
(exit)

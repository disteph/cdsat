(set-logic FO)
(set-info :status unsat)
(declare-fun p0 (Term) Bool)
(declare-fun p1 (Term) Bool)
(declare-fun p2 (Term) Bool)
(declare-fun p3 (Term) Bool)
(declare-fun p4 (Term) Bool)
(declare-fun p5 (Term) Bool)
(declare-fun q0 (Term) Bool)
(declare-fun q1 (Term) Bool)
(declare-fun s (Term Term) Bool)
(declare-fun r (Term Term) Bool)
(assert (forall ((x Term)) (and (=> (p1 x) (p0 x)) (exists ((x Term)) (p1 x)))))
(assert (forall ((x Term)) (and (=> (p2 x) (p0 x)) (exists ((x Term)) (p2 x)))))
(assert (forall ((x Term)) (and (=> (p3 x) (p0 x)) (exists ((x Term)) (p3 x)))))
(assert (forall ((x Term)) (and (=> (p4 x) (p0 x)) (exists ((x Term)) (p4 x)))))
(assert (forall ((x Term)) (and (=> (p5 x) (p0 x)) (exists ((x Term)) (p5 x)))))
(assert (and (exists ((x Term)) (q1 x)) (forall ((x Term)) (=> (q1 x) (q0 x)))))
(assert (forall ((x Term)) (=> (p0 x) (or (forall ((y Term)) (=> (q0 y) (r x y))) (forall ((y Term)) (=> (and (p0 y) (and (s x y) (exists ((z Term)) (and (q0 z) (r y z))))) (r x y)))))))
(assert (forall ((x Term) (y Term)) (=> (and (p3 y) (or (p5 x) (p4 x))) (s x y))))
(assert (forall ((x Term) (y Term)) (=> (and (p3 x) (p2 y)) (s x y))))
(assert (forall ((x Term) (y Term)) (=> (and (p2 x) (p1 y)) (s x y))))
(assert (forall ((x Term) (y Term)) (=> (and (p1 x) (or (p2 y) (q1 x))) (not (r x y)))))
(assert (forall ((x Term) (y Term)) (=> (and (p3 x) (p4 y)) (r x y))))
(assert (forall ((x Term) (y Term)) (=> (and (p3 x) (p5 y)) (not (r x y)))))
(assert (forall ((x Term)) (=> (or (p4 x) (p5 x)) (exists ((y Term)) (and (q0 y) (r x y))))))
(assert (not (exists ((x Term) (y Term)) (and (p0 x) (and (p0 y) (exists ((z Term)) (and (q1 z) (and (r y z) (r x y)))))))))
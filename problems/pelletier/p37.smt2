(set-logic FO)
(set-info :status unsat)
(declare-fun p (Term Term) Bool)
(declare-fun q (Term Term) Bool)
(declare-fun r (Term Term) Bool)
(assert (forall ((z Term))
        (exists ((w Term)) 
        (forall ((x Term)) 
        (exists ((y Term)) 
        (and 
             (=> (p x z) (p y w)) 
             (and 
                  (p y z) 
                  (=> 
                      (p y w) 
                      (exists ((u Term)) (q u w))
                      ))))))))
(assert (forall ((x Term) (z Term))
                (=> 
                    (not (p x z)) 
                    (exists ((y Term)) (q y z)))))
(assert (=> 
            (exists ((x Term) (y Term))(q x y)) 
            (forall ((x Term)) (r x x))))
(assert (not 
             (forall ((x Term)) 
             (exists ((y Term)) 
             (r x y)))))
(check-sat)
(exit)
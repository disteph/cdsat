(set-logic QF_UF)
(set-info :status unsat)
(declare-sort Term 0)
(declare-fun f (Term Term) Term)
(declare-fun a () Term)
(declare-fun b () Term)
(declare-fun c () Term)
(declare-fun d () Term)
(declare-fun e () Term)
(declare-fun g () Term)
(declare-fun h () Term)
(assert (and (and (and (= (f g h) d) (and (= c d) (= (f g d) a))) (and (= e c) (and (= e b) (= b h)))) (not (= a h))))
(check-sat)
(exit)
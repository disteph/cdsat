(set-logic QF_UF)
(set-info :source |
CADE ATP System competition. See http://www.cs.miami.edu/~tptp/CASC
 for more information. 

This benchmark was obtained by trying to find a finite model of a first-order 
formula (Albert Oliveras).
|)
(set-info :smt-lib-version 2.0)
(set-info :category "crafted")
(set-info :status unsat)
(declare-sort U 0)
(declare-fun c3 () U)
(declare-fun f1 (U U) U)
(declare-fun c2 () U)
(declare-fun f4 (U) U)
(declare-fun c_0 () U)
(declare-fun c_1 () U)
(assert (let ((?v_1 (f1 c3 c_0))) (let ((?v_0 (f1 ?v_1 c_0)) (?v_2 (f1 c_0 c_0))) (let ((?v_10 (f1 c_0 ?v_2)) (?v_4 (f1 c_0 c_1)) (?v_3 (f1 ?v_1 c_1)) (?v_13 (f1 c_1 ?v_2)) (?v_6 (f1 c3 c_1))) (let ((?v_5 (f1 ?v_6 c_0)) (?v_7 (f1 c_1 c_0)) (?v_9 (f1 c_1 c_1))) (let ((?v_12 (f1 c_0 ?v_9)) (?v_8 (f1 ?v_6 c_1)) (?v_15 (f1 c_1 ?v_9)) (?v_11 (f1 c2 c_0)) (?v_14 (f1 c2 c_1)) (?v_16 (f4 c_0))) (let ((?v_17 (f1 c_0 ?v_16)) (?v_18 (f4 c_1))) (let ((?v_19 (f1 c_1 ?v_18))) (and (distinct c_0 c_1) (= (f1 ?v_0 c_0) ?v_10) (= (f1 ?v_0 c_1) (f1 c_0 ?v_4)) (= (f1 ?v_3 c_0) ?v_13) (= (f1 ?v_3 c_1) (f1 c_1 ?v_4)) (= (f1 ?v_5 c_0) (f1 c_0 ?v_7)) (= (f1 ?v_5 c_1) ?v_12) (= (f1 ?v_8 c_0) (f1 c_1 ?v_7)) (= (f1 ?v_8 c_1) ?v_15) (= (f1 ?v_11 c_0) ?v_10) (= (f1 ?v_11 c_1) ?v_12) (= (f1 ?v_14 c_0) ?v_13) (= (f1 ?v_14 c_1) ?v_15) (not (= ?v_17 (f1 ?v_16 ?v_17))) (not (= ?v_19 (f1 ?v_18 ?v_19))) (or (= ?v_2 c_0) (= ?v_2 c_1)) (or (= ?v_4 c_0) (= ?v_4 c_1)) (or (= ?v_7 c_0) (= ?v_7 c_1)) (or (= ?v_9 c_0) (= ?v_9 c_1)) (or (= ?v_16 c_0) (= ?v_16 c_1)) (or (= ?v_18 c_0) (= ?v_18 c_1)) (or (= c3 c_0) (= c3 c_1)) (or (= c2 c_0) (= c2 c_1)))))))))))
(check-sat)
(exit)

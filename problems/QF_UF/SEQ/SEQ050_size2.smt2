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
(declare-fun c4 () U)
(declare-fun f1 (U) U)
(declare-fun c5 () U)
(declare-fun f2 (U) U)
(declare-fun c6 () U)
(declare-fun f3 (U U) U)
(declare-fun c7 () U)
(declare-fun c_0 () U)
(declare-fun c_1 () U)
(assert (let ((?v_0 (f1 c4)) (?v_1 (f1 c_0)) (?v_2 (f1 c_1)) (?v_6 (f3 c_0 c_0)) (?v_3 (f2 c_0))) (let ((?v_5 (not (= ?v_1 ?v_3))) (?v_8 (f3 c_0 c_1)) (?v_4 (f2 c_1))) (let ((?v_7 (not (= ?v_1 ?v_4))) (?v_10 (f3 c_1 c_0)) (?v_9 (not (= ?v_2 ?v_3))) (?v_12 (f3 c_1 c_1)) (?v_11 (not (= ?v_2 ?v_4))) (?v_13 (f3 c4 c5))) (let ((?v_17 (f3 c_0 ?v_13))) (let ((?v_14 (not (= ?v_17 c_0))) (?v_16 (f2 ?v_13))) (let ((?v_15 (not (= ?v_1 ?v_16))) (?v_19 (= c_0 c_0)) (?v_20 (f3 c_1 ?v_13))) (let ((?v_23 (not (= ?v_20 c_0))) (?v_21 (= c_1 c_0)) (?v_22 (not (= ?v_2 ?v_16))) (?v_18 (not (= ?v_17 c_1))) (?v_24 (not (= ?v_20 c_1))) (?v_25 (= c_0 c_1)) (?v_26 (= c_1 c_1))) (and (distinct c_0 c_1) (= ?v_0 (f2 c5)) (= ?v_0 (f2 c6)) (= (f2 ?v_1) ?v_1) (= (f2 ?v_2) ?v_2) (or (= (f2 ?v_6) ?v_3) ?v_5) (or (= (f2 ?v_8) ?v_3) ?v_7) (or (= (f2 ?v_10) ?v_4) ?v_9) (or (= (f2 ?v_12) ?v_4) ?v_11) (= (f3 c4 c6) (f3 c4 c7)) (= ?v_0 (f2 c7)) (or ?v_5 (= (f1 ?v_6) ?v_1)) (or ?v_7 (= (f1 ?v_8) ?v_2)) (or ?v_9 (= (f1 ?v_10) ?v_1)) (or ?v_11 (= (f1 ?v_12) ?v_2)) (not (= c6 c7)) (= (f3 ?v_3 c_0) c_0) (= (f3 ?v_4 c_1) c_1) (or ?v_5 ?v_5 (= (f3 c_0 ?v_6) (f3 ?v_6 c_0))) (or ?v_5 ?v_7 (= (f3 c_0 ?v_8) (f3 ?v_6 c_1))) (or ?v_7 ?v_9 (= (f3 c_0 ?v_10) (f3 ?v_8 c_0))) (or ?v_7 ?v_11 (= (f3 c_0 ?v_12) (f3 ?v_8 c_1))) (or ?v_9 ?v_5 (= (f3 c_1 ?v_6) (f3 ?v_10 c_0))) (or ?v_9 ?v_7 (= (f3 c_1 ?v_8) (f3 ?v_10 c_1))) (or ?v_11 ?v_9 (= (f3 c_1 ?v_10) (f3 ?v_12 c_0))) (or ?v_11 ?v_11 (= (f3 c_1 ?v_12) (f3 ?v_12 c_1))) (= (f1 ?v_3) ?v_3) (= (f1 ?v_4) ?v_4) (or ?v_14 ?v_15 ?v_14 ?v_19 ?v_15) (or ?v_14 ?v_15 ?v_23 ?v_21 ?v_22) (or ?v_18 ?v_15 ?v_18 ?v_19 ?v_15) (or ?v_18 ?v_15 ?v_24 ?v_21 ?v_22) (or ?v_23 ?v_22 ?v_14 ?v_25 ?v_15) (or ?v_23 ?v_22 ?v_23 ?v_26 ?v_22) (or ?v_24 ?v_22 ?v_18 ?v_25 ?v_15) (or ?v_24 ?v_22 ?v_24 ?v_26 ?v_22) (= (f3 c_0 ?v_1) c_0) (= (f3 c_1 ?v_2) c_1) (or (= ?v_6 c_0) (= ?v_6 c_1)) (or (= ?v_8 c_0) (= ?v_8 c_1)) (or (= ?v_10 c_0) (= ?v_10 c_1)) (or (= ?v_12 c_0) (= ?v_12 c_1)) (or (= ?v_1 c_0) (= ?v_1 c_1)) (or (= ?v_2 c_0) (= ?v_2 c_1)) (or (= ?v_3 c_0) (= ?v_3 c_1)) (or (= ?v_4 c_0) (= ?v_4 c_1)) (or (= c4 c_0) (= c4 c_1)) (or (= c5 c_0) (= c5 c_1)) (or (= c6 c_0) (= c6 c_1)) (or (= c7 c_0) (= c7 c_1)))))))))))
(check-sat)
(exit)

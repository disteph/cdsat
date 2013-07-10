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
(declare-fun f3 (U U) U)
(declare-fun f2 (U) U)
(declare-fun f1 (U) U)
(declare-fun c6 () U)
(declare-fun c4 () U)
(declare-fun c5 () U)
(declare-fun c7 () U)
(declare-fun c_0 () U)
(declare-fun c_1 () U)
(assert (let ((?v_22 (f3 c_0 c_0)) (?v_0 (f2 c_0)) (?v_1 (f1 c_0))) (let ((?v_21 (not (= ?v_1 ?v_0))) (?v_24 (f3 c_0 c_1)) (?v_2 (f2 c_1))) (let ((?v_23 (not (= ?v_1 ?v_2))) (?v_26 (f3 c_1 c_0)) (?v_3 (f1 c_1))) (let ((?v_25 (not (= ?v_3 ?v_0))) (?v_28 (f3 c_1 c_1)) (?v_27 (not (= ?v_3 ?v_2))) (?v_4 (f3 c4 c5)) (?v_6 (= c_0 c_0)) (?v_7 (f3 c_0 c5))) (let ((?v_5 (not (= ?v_7 c_0))) (?v_16 (f2 c4))) (let ((?v_8 (not (= ?v_1 ?v_16))) (?v_11 (f2 c5))) (let ((?v_9 (not (= ?v_1 ?v_11))) (?v_10 (not (= ?v_7 c_1))) (?v_12 (= c_0 c_1)) (?v_13 (not (= ?v_3 ?v_11))) (?v_14 (f3 c_1 c5))) (let ((?v_15 (not (= ?v_14 c_0))) (?v_18 (not (= ?v_14 c_1))) (?v_17 (= c_1 c_0)) (?v_19 (not (= ?v_3 ?v_16))) (?v_20 (= c_1 c_1)) (?v_30 (f3 c_0 c4))) (let ((?v_29 (not (= ?v_30 c_0))) (?v_31 (not (= ?v_30 c_1))) (?v_32 (f3 c_1 c4))) (let ((?v_33 (not (= ?v_32 c_0))) (?v_34 (not (= ?v_32 c_1))) (?v_35 (f2 ?v_4))) (and (distinct c_0 c_1) (or (= (f2 ?v_22) ?v_0) ?v_21) (or (= (f2 ?v_24) ?v_0) ?v_23) (or (= (f2 ?v_26) ?v_2) ?v_25) (or (= (f2 ?v_28) ?v_2) ?v_27) (= (f1 ?v_0) ?v_0) (= (f1 ?v_2) ?v_2) (= (f3 ?v_0 c_0) c_0) (= (f3 ?v_2 c_1) c_1) (= (f3 c6 ?v_4) (f3 c7 ?v_4)) (or ?v_6 ?v_5 ?v_8 ?v_9 ?v_5) (or ?v_6 ?v_10 ?v_8 ?v_9 ?v_10) (or ?v_12 ?v_5 ?v_8 ?v_13 ?v_15) (or ?v_12 ?v_10 ?v_8 ?v_13 ?v_18) (or ?v_17 ?v_15 ?v_19 ?v_9 ?v_5) (or ?v_17 ?v_18 ?v_19 ?v_9 ?v_10) (or ?v_20 ?v_15 ?v_19 ?v_13 ?v_15) (or ?v_20 ?v_18 ?v_19 ?v_13 ?v_18) (= (f2 ?v_1) ?v_1) (= (f2 ?v_3) ?v_3) (or ?v_21 (= (f1 ?v_22) ?v_1)) (or ?v_23 (= (f1 ?v_24) ?v_3)) (or ?v_25 (= (f1 ?v_26) ?v_1)) (or ?v_27 (= (f1 ?v_28) ?v_3)) (or ?v_6 ?v_8 ?v_29 ?v_29 ?v_9) (or ?v_6 ?v_8 ?v_31 ?v_31 ?v_9) (or ?v_12 ?v_8 ?v_33 ?v_29 ?v_13) (or ?v_12 ?v_8 ?v_34 ?v_31 ?v_13) (or ?v_17 ?v_19 ?v_29 ?v_33 ?v_9) (or ?v_17 ?v_19 ?v_31 ?v_34 ?v_9) (or ?v_20 ?v_19 ?v_33 ?v_33 ?v_13) (or ?v_20 ?v_19 ?v_34 ?v_34 ?v_13) (= (f1 c6) ?v_35) (= (f1 c4) ?v_11) (= (f1 c7) ?v_35) (= (f3 c_0 ?v_1) c_0) (= (f3 c_1 ?v_3) c_1) (or (= (f3 c_0 ?v_22) (f3 ?v_22 c_0)) ?v_21 ?v_21) (or (= (f3 c_0 ?v_24) (f3 ?v_22 c_1)) ?v_21 ?v_23) (or (= (f3 c_0 ?v_26) (f3 ?v_24 c_0)) ?v_23 ?v_25) (or (= (f3 c_0 ?v_28) (f3 ?v_24 c_1)) ?v_23 ?v_27) (or (= (f3 c_1 ?v_22) (f3 ?v_26 c_0)) ?v_25 ?v_21) (or (= (f3 c_1 ?v_24) (f3 ?v_26 c_1)) ?v_25 ?v_23) (or (= (f3 c_1 ?v_26) (f3 ?v_28 c_0)) ?v_27 ?v_25) (or (= (f3 c_1 ?v_28) (f3 ?v_28 c_1)) ?v_27 ?v_27) (not (= c6 c7)) (or (= ?v_22 c_0) (= ?v_22 c_1)) (or (= ?v_24 c_0) (= ?v_24 c_1)) (or (= ?v_26 c_0) (= ?v_26 c_1)) (or (= ?v_28 c_0) (= ?v_28 c_1)) (or (= ?v_0 c_0) (= ?v_0 c_1)) (or (= ?v_2 c_0) (= ?v_2 c_1)) (or (= ?v_1 c_0) (= ?v_1 c_1)) (or (= ?v_3 c_0) (= ?v_3 c_1)) (or (= c6 c_0) (= c6 c_1)) (or (= c4 c_0) (= c4 c_1)) (or (= c5 c_0) (= c5 c_1)) (or (= c7 c_0) (= c7 c_1))))))))))))))
(check-sat)
(exit)

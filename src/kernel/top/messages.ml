(*****************)
(* Message types *)
(*****************)

open Format

open Interfaces_basic
open Basic

exception ModelError of string

(* Useful abbreviation for term type *)

type 'a term = (IntSort.t,'a) Terms.term


type (_,_) message =
| Provable: 'sign*'tset                       -> ('sign,'tset) message
| NotProvable: 'sign*'tset                    -> ('sign,'tset) message
| Straight: 'sign*'tset*('tset->'tset)        -> ('sign,'tset) message
| AndNode : 'sign*'tset*'tset*('tset->'tset->'tset) -> ('sign,'tset) message
| OrNode  : 'sign*'tset*'tset*(bool->'tset->'tset)  -> ('sign,'tset) message


(* The type of answers that a theory should produce, when queried *)

type ('a,'b) answer = private 
                      | UNSAT of 'b
                      | SAT   of ('a,'b) resume
                      | GimmeFreshVar of Sorts.t*(World.FreeVar.t -> ('a,'b) resume)
                      | Write of 'b * (('a,'b) resume)
                          
and ('a,'b) resume = 'b -> (('a,'b) answer)

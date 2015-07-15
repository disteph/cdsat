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
| ThProvable: 'sign*'tset                       -> ('sign,'tset) message
| ThNotProvable: 'sign*'tset                    -> ('sign,'tset) message
| ThStraight: 'sign*'tset*('tset->'tset)        -> ('sign,'tset) message
| ThAnd : 'sign*'tset*'tset*('tset->'tset->'tset) -> ('sign,'tset) message
| ThOr  : 'sign*'tset*'tset*(bool->'tset->'tset)  -> ('sign,'tset) message

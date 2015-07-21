(*****************)
(* Message types *)
(*****************)

open Format

open Interfaces_basic
open Basic

exception ModelError of string

(* Useful abbreviation for term type *)

type 'a term = (IntSort.t,'a) Terms.term


type (_,_) thdone =
| ThProvable   : 'sign*'tset -> ('sign,'tset) thdone
| ThNotProvable: 'sign*'tset -> ('sign,'tset) thdone

type (_,_) thsays =
| ThStraight: 'sign*'tset*('tset->'tset)        -> ('sign,'tset) thsays
| ThAnd : 'sign*'tset*'tset*('tset->'tset->'tset) -> ('sign,'tset) thsays
| ThOr  : 'sign*'tset*'tset*(bool->'tset->'tset)  -> ('sign,'tset) thsays

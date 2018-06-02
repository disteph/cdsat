(**********************************************************)
(* This file contains the implementation of HConsed types *)
(**********************************************************)

include module type of HCons_sig

module MakePoly(M: sig type ('recurs,'a) t end) : PolyS with type ('t,'a) initial := ('t,'a) M.t

module Make(M: sig type 'a t end) : S with type 't initial := 't M.t

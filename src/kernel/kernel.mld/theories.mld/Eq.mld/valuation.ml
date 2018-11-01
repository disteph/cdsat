open General
open Patricia_tools

open Top
open Terms
open Sassigns
open Values

(* Valuations are term -> values maps extracted from the Egraph *)
(* We define a notion of valuation,
   as a map from terms to the combined values found in the egraph. *)
module Arg = struct
  include Term
  include TypesFromHConsed(Term)
  include EmptyInfo
  type values = CValue.t * (Assign.t*int) Lazy.t
end

include Patricia.Map.MakeNH(Arg)

let pp_pair fmt (term,(cval,_)) =
  Format.fprintf fmt "(%a↦ %a)" Term.pp term CValue.pp cval
let pp = print_in_fmt ~wrap:("{","}") pp_pair

type 'sign signed = t

let sempty = empty
let sunion = union (fun _ _ _ -> failwith "Valuation union should be disjoint")
let build _ t = t
let reveal t  = t

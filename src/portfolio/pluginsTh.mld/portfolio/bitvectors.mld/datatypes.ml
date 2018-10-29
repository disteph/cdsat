open Format
       
open General
open Patricia
open Patricia_tools

open Kernel
open Top
open Terms
open Sassigns
       
module Arg = struct
  include Term
  include TypesFromHConsed(Term)
  include EmptyInfo
  type values = Assign.t Range.t
end

module Domain = struct

  include Map.MakeNH(Arg)

  let pp fmt domains =
    let pp_binding fmt (var,range) =
      fprintf fmt "%a ∈ %a" Arg.pp var Range.pp range
    in
    let rec pp_aux fmt = function
      | [] -> ()
      | [binding] -> pp_binding fmt binding
      | binding::tail -> fprintf fmt " %a\n %a" pp_binding binding pp_aux tail
    in pp_aux fmt (elements domains)

end


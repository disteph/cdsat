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

module ConfigB = struct

  module Constraint = struct
    type t = {
      sassign   : SAssign.t;
      variables : TSet.t [@opaque]
    } [@@deriving show]
    let id {sassign} = SAssign.id sassign
  end

  module Var = Term

  type fixed = K.Model.t

  let simplify fixed (c,b) = K.Simpl.simplify fixed c, b

  let pick_another _ (c,_) i _ =
    Print.print ["LRA",2] (fun p ->
        p "LRA: WLB picks variables for %a, gets %a"
          K.Simpl.pp c
          (List.pp Term.pp) (K.Simpl.watchable c));
    K.Simpl.watchable c
end

module WLB = TwoWatchedLits.Make(ConfigB)

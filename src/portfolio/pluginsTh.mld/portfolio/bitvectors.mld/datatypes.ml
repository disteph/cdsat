open Format
       
open General
open Patricia
open Patricia_tools

open Kernel
open Export
open Theories.Bitvectors
open Top.Sassigns
open Top.Messages
       
module Make(DS: GlobalImplem)
         (K: MyTheory.API with type assign = DS.Assign.t
                               and type termdata= DS.Term.datatype
                               and type tset   = DS.TSet.t )
  = struct

  open DS
  type datatypes = Term.datatype*Value.t*Assign.t*TSet.t

  module QVar = struct
    type t = int [@@deriving ord]
    let id i = i
    let pp fmt i = Term.pp fmt (Term.term_of_id i)
    type values = Assign.t Range.t
    include EmptyInfo
    let treeHCons = None
  end

  module Domain = struct

    include PatMap.Make(QVar)(TypesFromHConsed(QVar))

    let pp fmt domains =
      let pp_binding fmt (var,range) =
        fprintf fmt "%a ∈ %a" QVar.pp var Range.pp range
      in
      let rec pp_aux fmt = function
        | [] -> ()
        | [binding] -> pp_binding fmt binding
        | binding::tail -> fprintf fmt " %a\n %a" pp_binding binding pp_aux tail
      in pp_aux fmt (elements domains)

  end

end

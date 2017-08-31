open Format
       
open General
open Patricia
open Patricia_tools

open Kernel
open Export
open Theories.LRA
open Top.Sassigns
open Top.Messages
       
open Tools.PluginsTh
       
module Make(DS: GlobalImplem)
         (K: API.API with type sign   = MyTheory.sign
                      and type assign = DS.Assign.t
                      and type termdata= DS.Term.datatype
                      and type value  = DS.Value.t
                      and type tset   = DS.TSet.t )
  = struct

  open DS
  type datatypes = Term.datatype*Value.t*Assign.t*TSet.t


  module QVar = struct
    type t = int [@@deriving ord]
    let id i = i
    let pp fmt i = Term.pp fmt (Term.term_of_id i)
    type values = bassign Range.t
    include MaxInfo(struct type t = int [@@deriving ord] end)
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


  module ConfigB = struct

    module Constraint = struct
      type t = K.Simpl.t * bool [@@deriving show]
      let id (c,b) =
        let t = K.Simpl.term c in
        2*(Term.id t)+(if b then 1 else 0)
    end

    module Var = struct
      type t = int [@@deriving ord]
      let pp fmt i = Term.pp fmt (Term.term_of_id i)
      let show = Print.stringOf pp
    end

    type fixed = K.Model.t

    let simplify fixed (c,b) = K.Simpl.simplify fixed c, b

    let pick_another _ (c,_) i _ =
      Print.print ["LRA",2] (fun p ->
          p "LRA: WLB picks variables for %a, gets %a"
            K.Simpl.pp c
            (List.pp Term.pp)
            (List.map Term.term_of_id (K.Simpl.watchable c)));
      K.Simpl.watchable c
  end

  module WLB = TwoWatchedLits.Make(ConfigB)


  module ConfigQ = struct

    module Constraint = struct
      type t = K.Simpl.t * (Top.Qhashed.t option) * int [@@deriving show]
      let id (_,_,i) = i
    end

    module Var = struct
      type t = int [@@deriving ord]
      let pp fmt i = Term.pp fmt (Term.term_of_id i)
      let show = Print.stringOf pp
    end

    type fixed = K.Model.t

    let simplify fixed (c,v,i) = K.Simpl.simplify fixed c,v,i

    let pick_another _ (c,_,_) i _ =
      Print.print ["LRA",2] (fun p ->
          p "LRA: WLQ picks variables for %a, gets %a"
            K.Simpl.pp c
            (List.pp Term.pp)
            (List.map Term.term_of_id (K.Simpl.watchable c)));
      K.Simpl.watchable c
  end

  module WLQ = TwoWatchedLits.Make(ConfigQ)

  (* pretty printing an evaluation inference *)
  let pp_beval fmt msg =
    let Propa(justif,Straight(t,Top.Values.Boolean b)) = msg in
    Format.fprintf fmt "%a ⊢ %a"
      Assign.pp justif
      (Top.Sassigns.pp_sassign K.Simpl.pp Q.pp_print)
      (SAssign(K.Simpl.make t,Top.Values.Boolean b))      

end

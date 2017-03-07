open Format

open Top
open Interfaces_basic
open Basic
open Symbols
open Specs
open Variables

open Prop
open Literals
open Formulae
open Interfaces_theory

module Make(PropDS:DataType) = struct

  module DS = struct

    module DT = struct

      type t = LitF.t*MakesSense.t

      let bV tag fv = LitF.build(true,tag), MakesSense.fv fv

      let rec list_collect a = 
        List.fold (fun (_,ms) sofar -> MakesSense.combine sofar ms) a MakesSense.init

      let negation (a,ms) = LitF.negation a,ms

      let bC_aux symb l =
        match symb, l with
        | Neg,[ams] -> negation ams
        | _ -> raise (ModelError "ModelError: semantic_aux does not know symbol")

      let bC tag symb l =
        (* print_endline(string_of_int id); *)
        try bC_aux symb l
        with ModelError _
          -> LitF.build(true,tag), list_collect l
    end

    open DT

    module Term = Terms.Make(FreeVar)(Pairing(PropDS)(DT))

    type formulae = PropDS.t
    let asF = fst

    let asL term = let l,ms = snd(Terms.data term) in
                   (* print_endline (Dump.toString (fun p -> p "Lit: %a" LitF.print_in_fmt l)); *)
                   let b,id = LitF.reveal l in
                   let atom = Term.term_of_id id in
                   (* print_endline (Dump.toString (fun p -> p "Term: %a" Term.print_in_fmt atom)); *)
                   b,atom,ms

    module TSet = MakeCollection(struct include Term let compare = Terms.compare end)

    let makes_sense term = MakesSense.check (snd(snd(Terms.data term)))

    module Constraint = Constraint

  end

  open DS

  exception FoundIt of TSet.t*Constraint.t*(TSet.t,Constraint.t) stream

(* We range over atomN,
   alias is the set of atoms that haven't been tried (starts with atomN);
   as soon as we find a solution, we raise an exception *)

  let aux aset compare cont t atomN sigma = 
    TSet.fold
      (fun a alias -> 
        Dump.print ["FirstOrder",1] (fun p -> p "Unifying literals %a and %a" Term.print_in_fmt a Term.print_in_fmt t);
        let newalias = TSet.remove a alias in
        (match asL a, asL t with
        | (b1,l1,_), (b2,l2,_) when compare b1 b2 ->
          (Dump.print ["FirstOrder",1] (fun p -> p "constraint = %a" Constraint.print_in_fmt sigma);
           Dump.print ["FirstOrder",1] (fun p -> p "Actually unifying atoms %a and %a" Term.print_in_fmt l1 Term.print_in_fmt l2);
           match Constraint.unif sigma l1 l2 with
           | Some newsigma -> 
             raise (FoundIt(TSet.add a aset,newsigma, cont newalias))
           | None -> ())
        | _ -> ());
        newalias)
      atomN 
      atomN 
      
  let rec goal_consistency t atomN sigma = 
  (* (print_endline (Dump.toString(fun p->p "goal consistency on %a |- %a" TSet.print_in_fmt atomN))); *)
  (* We range over atomN,
     we catch an exception (means success);
     otherwise we finish with NoMore *)
    try 
      let _ = 
        aux TSet.empty [%eq:bool] (goal_consistency t) t atomN sigma
      in NoMore
    with
      FoundIt(a,newsigma,f) -> Guard(a,newsigma,f)

  let rec consistency atomN sigma =
    try 
      let _ =
        TSet.fold
          (fun t after -> 
            let newafter = TSet.remove t after in
            let aset = TSet.add t TSet.empty in
            let rec g_consistency remaining sigma =
              try 
                let _ = aux aset (!=) g_consistency t remaining sigma in
                consistency newafter sigma
              with
                FoundIt(a,newsigma,f) -> Guard(a,newsigma,f)
            in
            let _ = aux aset (!=) g_consistency t newafter sigma in newafter
          )
          atomN 
          atomN 
      in
      NoMore
    with
      FoundIt(a,newsigma,f) -> Guard(a,newsigma,f)

end

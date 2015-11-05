open Top
open Messages
open Specs

type sign = unit

(* Kind of merge what is done in cc_pl1.ml and MyTheory.Make for CC *)
(* We should mainly implement the add function here *)
module Make(DS: sig
  include GTheoryDSType
  val proj: Term.datatype -> ThDS.t
end) = struct

  open DS

  (* Probably to be reorganized later... *)

  (* Get terms back from hashkey and obtain a tailState => To-do*)
  let atoI a =
    let b,t = LitF.reveal(proj(Terms.data a)) in
    let a = Term.term_of_id t in
    match Terms.reveal a with
    | Terms.C(Symbols.Eq so,[a1;a2]) when b -> Eq(so,a1,a2)
    | Terms.C(Symbols.Eq so,[a1;a2])        -> NEq(so,a1,a2)
    | Terms.C(Symbols.NEq so,[a1;a2]) when b-> NEq(so,a1,a2)
    | Terms.C(Symbols.NEq so,[a1;a2])       -> Eq(so,a1,a2)
    | _ when get_sort a = Sorts.Prop -> Eq(Sorts.Prop, a, Term.bC (if b then Symbols.True else Symbols.False) [])
    | _ -> assert false


  (*****************************************)

  let rec state atomN
      = (module struct

        type newoutput = (sign,TSet.t) output
        type tset = TSet.t

        let treated = (fun () -> atomN)

        let add tset =
            let newtreated = TSet.union treated tset in
            (* appliquer l'algo sur l'ensemble des clauses *)
            (* fonction representant l'algo et prenant en param un objet état*)
            (* cet objet contient les inégalités à traiter *)
            (* il va correspondre à un niveau de la "route" de l'algo de Dejan*)
            (* dejanSolver initalState *)
            Output(None,state atomN)

        let normalise = (fun _ -> failwith "Not a theory with normaliser")

        let clone = (fun () -> Output(None, state atomN))
      end : SlotMachine with type newoutput = (sign,TSet.t) output and type tset = TSet.t)

  (* Only thing requierd by signature*)
  let init = state TSet.empty

end

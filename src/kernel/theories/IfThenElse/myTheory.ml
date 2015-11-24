open Top
open Messages
open Specs

open Prop
open Literals

type sign = unit

module LMap = Map.Make(LitF)

module Make
  (DS: sig 
    include GTheoryDSType
    val proj: Term.datatype -> LitF.t
  end) =
struct

  open DS

  type state = {
    treated: TSet.t;
    known: Term.t LMap.t;
    todo: Term.t list;
  }

  let rec print_in_fmtL fmt h = 
    LMap.fold
      (fun blit b () ->
        Format.fprintf fmt "(%a,%a)," LitF.print_in_fmt blit Term.print_in_fmt b
      )
      h 
      ()

  let rec print_in_fmt fmt = function
    | [] -> Format.fprintf fmt "[]"
    | t::l -> Format.fprintf fmt "%a::%a" Term.print_in_fmt t print_in_fmt l

  let rec machine state
      = (module struct

        type newoutput = (sign,TSet.t) output
        type tset = TSet.t

        let treated() = state.treated

        let add newlits =
          let state = 
            match newlits with
            | None    -> state
            | Some nl ->
               { 
                 treated = TSet.union state.treated nl;
                 known   = TSet.fold (fun t -> LMap.add (proj(Terms.data t)) t) nl state.known;
                 todo    = TSet.fold (fun t l -> t::l) nl state.todo
               }
          in
          let rec aux = function
            | []   -> 
               (Output(
                 Some(thNotProvable () state.treated),
                 machine { state with todo = [] }
               ):(sign,TSet.t) output)
            | t::l ->
               begin match Terms.reveal t with
               | Terms.C(Symbols.ITE so,[b;b1;b2]) 
                 ->
                  let blit = proj(Terms.data b) in
                  if LMap.mem blit state.known
                  then
                    (* (Dump.msg (Some(fun p -> p "Condition (%a,%a) seen" LitF.print_in_fmt blit Term.print_in_fmt b)) None None; *)
                    let b0 = LMap.find blit state.known in
                    let eq = Term.bC (Symbols.Eq so) [t;b1] in
                    Output(
                      Some(thStraight () (TSet.add eq TSet.empty) (TSet.add b0 TSet.empty)),
                      machine { state with todo = l })
               (* ) *)
                  else 
                    if LMap.mem (LitF.negation blit) state.known
                    then
                      (* (Dump.msg (Some(fun p -> p "Condition -(%a,%a) seen" LitF.print_in_fmt blit  Term.print_in_fmt b)) None None; *)
                      let b0 = LMap.find (LitF.negation blit) state.known in
                      let eq = Term.bC (Symbols.Eq so) [t;b2] in
                      Output(
                        Some(thStraight () (TSet.add eq TSet.empty) (TSet.add b0 TSet.empty)),
                        machine { state with todo = l })
                    (* ) *)
                    else
                      (* (Dump.msg (Some(fun p -> p "Condition (%a,%a) not seen" LitF.print_in_fmt blit  Term.print_in_fmt b)) None None; *)
                      Output(
                        Some(
                          thAnd () (TSet.add b TSet.empty) (TSet.add (Term.bC Symbols.Neg [b]) TSet.empty) TSet.empty
                        ),
                        machine { state with todo = t::l })
               (* ) *)

               | Terms.C(Symbols.Eq _,[a1;a2])
               | Terms.C(Symbols.NEq _,[a1;a2])
                 -> aux (a1::a2::l)

               | _ -> aux l

               end
          in
          (* Dump.msg (Some(fun p -> p "treated=%a" TSet.print_in_fmt state.treated)) None None; *)
          (* Dump.msg (Some(fun p -> p "known=%a" print_in_fmtL state.known)) None None; *)
          (* Dump.msg (Some(fun p -> p "todo=%a" print_in_fmt state.todo)) None None; *)
          aux state.todo

        let normalise = (fun _ -> failwith "Not a theory with normaliser")

        let clone = (fun () -> Output(None, machine state))

      end : SlotMachine with type newoutput = (sign,TSet.t) output and type tset = TSet.t)

  let init = machine { treated = TSet.empty; known = LMap.empty; todo = [] }

end

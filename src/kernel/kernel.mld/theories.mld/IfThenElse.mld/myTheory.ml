open Top
open Messages
open Specs

open Termstructures.Literals

type sign = unit

type ts = LitF.t
let ts  = Termstructures.Register.LitF

include Theory.HasNoValues

module LMap = Map.Make(LitF)

module Make(DS: DSproj with type ts = LitF.t) = struct

  type assign = DS.Assign.t
  open DS

  type state = {
    treated: Assign.t;
    known: Term.t LMap.t;
    todo: Term.t list;
    solved: Assign.t;
  }

  let rec print_in_fmtL fmt h = 
    LMap.fold
      (fun blit b () ->
        Format.fprintf fmt "(%a,%a)," (LitF.print_in_fmt ~print_atom:Term.print_of_id) blit Term.pp b
      )
      h 
      ()

  let rec pp fmt = function
    | [] -> Format.fprintf fmt "[]"
    | t::l -> Format.fprintf fmt "%a::%a" Term.pp t pp l

  let rec machine state =
    (module struct
       
       type newoutput = (sign,Assign.t) output
       type tset = Assign.t

       let add newlits =
         let state = 
           match newlits with
           | None    -> state
           | Some nl ->
              { 
                treated = Assign.union state.treated nl;
                known   = Assign.fold (fun t -> LMap.add (proj(Terms.data t)) t) nl state.known;
                todo    = Assign.fold (fun t l -> t::l) nl state.todo;
                solved  = state.solved
              }
         in
         let rec aux = function
           | []   -> 
              (Output(
                   Some(sat () state.treated),
                   machine { state with todo = [] }
                 ):(sign,Assign.t) output)
           | t::l when Assign.mem t state.solved -> aux l
           | t::l ->
              begin match Terms.reveal t with
              | Terms.C(Symbols.ITE so,[b;b1;b2]) 
                ->
                 let blit = proj(Terms.data b) in
                 if LMap.mem blit state.known
                 then
                   (* (Dump.print ["IfThenElse",1] (fun p -> p "Condition (%a,%a) seen" LitF.pp blit Term.pp b); *)
                   let b0 = LMap.find blit state.known in
                   let eq = Term.bC (Symbols.Eq so) [t;b1] in
                   Output(
                       Some(straight () (Assign.singleton b0) (Assign.singleton eq)),
                       machine { state with todo = l; solved = Assign.add t state.solved })
                         (* ) *)
                 else 
                   if LMap.mem (LitF.negation blit) state.known
                   then
                     (* (Dump.print ["IfThenElse",1] (fun p -> p "Condition -(%a,%a) seen" LitF.pp blit  Term.pp b); *)
                     let b0 = LMap.find (LitF.negation blit) state.known in
                     let eq = Term.bC (Symbols.Eq so) [t;b2] in
                     Output(
                         Some(straight () (Assign.singleton b0) (Assign.singleton eq)),
                         machine { state with todo = l; solved = Assign.add t state.solved })
                           (* ) *)
                   else
                     (* (Dump.print ["IfThenElse",1] (fun p -> p "Condition (%a,%a) not seen" LitF.pp blit  Term.pp b); *)
                     Output(
                         Some(
                             both () Assign.empty (Assign.singleton b) (Assign.singleton (Term.bC Symbols.Neg [b]))
                           ),
                         machine { state with todo = t::l })
              (* ) *)

              | Terms.C(Symbols.Eq _,[a1;a2])
                | Terms.C(Symbols.NEq _,[a1;a2])
                -> aux (a1::a2::l)

              | _ -> aux l

              end
         in
         (* Dump.print ["IfThenElse",1] (fun p -> p "treated=%a" Assign.pp state.treated); *)
         (* Dump.print ["IfThenElse",1] (fun p -> p "known=%a" ppL state.known); *)
         (* Dump.print ["IfThenElse",1] (fun p -> p "todo=%a" pp state.todo); *)
         aux state.todo

       let normalise = (fun _ -> failwith "Not a theory with normaliser")

       let clone = (fun () -> Output(None, machine state))

       let suicide _ = ()

     end : SlotMachine with type newoutput = (sign,Assign.t) output and type tset = Assign.t)

  let init = machine { treated = Assign.empty; known = LMap.empty; todo = []; solved = Assign.empty }
  let clear () = ()
                   
end

module type API = sig
  type assign
  val init: (sign,assign) slot_machine
  val clear: unit -> unit
end

type ('t,'v,'a) api = (module API with type assign = 'a)

let make (type t)(type v)(type a)
      ((module DS): (ts,values,t,v,a) dsProj)
    : (t,v,a) api =
  (module Make(DS))

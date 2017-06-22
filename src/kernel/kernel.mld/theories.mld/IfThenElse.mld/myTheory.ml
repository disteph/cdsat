open Top
open Messages
open Specs

open Termstructures.Literals

type sign = unit

type ts = unit
let ts  = Termstructures.Register.NoRep

include Theory.HasNoValues

module Make(DS: DSproj) = struct

  open DS
  type assign = Assign.t
  module SAssign = Tools.SAssign(DS)
  type sassign = SAssign.t

  module TSet = Set.Make(Term)
  module TMap = Map.Make(Term)

  type state = {
    treated: Assign.t;
    known: bool TMap.t;
    todo: Term.t list;
    solved: TSet.t;
  }

  let rec machine state =
    (module struct
       
       type newoutput = (sign,Assign.t,SAssign.t) output
       type assign  = Assign.t
       type sassign = SAssign.t

       let add newa =
         let state = 
           match newa with
           | None    -> state
           | Some((t,v) as nl) ->
              { 
                treated = Assign.add nl state.treated;
                known   =
                  (match v with
                   | Values.Boolean b -> TMap.add t b state.known
                   | Values.NonBoolean _ -> state.known);
                todo    = t::state.todo;
                solved  = state.solved
              }
         in
         let rec aux = function
           | []   -> 
              (Output(
                   Some(sat () state.treated),
                   machine { state with todo = [] }
                 ):(sign,Assign.t,SAssign.t) output)
           | t::l when TSet.mem t state.solved -> aux l
           | t::l ->
              begin match Terms.reveal t with
              | Terms.C(Symbols.ITE so,[c;b1;b2]) 
                ->
                 if TMap.mem c state.known
                 then
                   let br = if TMap.find c state.known
                            then b1
                            else b2
                   in
                   (* (Dump.print ["IfThenElse",1] (fun p -> p "Condition (%a,%a) seen" LitF.pp blit Term.pp b); *)
                   let eq = Term.bC (Symbols.Eq so) [t;br] in
                   Output(
                       Some(straight () (Assign.singleton(Values.bassign c)) (eq,true)),
                       machine { state with todo = l; solved = TSet.add t state.solved })
                         (* ) *)
                 else
                   (* (Dump.print ["IfThenElse",1] (fun p -> p "Condition (%a,%a) not seen" LitF.pp blit  Term.pp b); *)
                   Output(
                       Some(both () Assign.empty (c,true) (c,false)),
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

     end : SlotMachine with type newoutput = (sign,Assign.t,SAssign.t) output
                        and type assign = Assign.t
                        and type sassign = SAssign.t)

  let init = machine { treated = Assign.empty;
                       known = TMap.empty;
                       todo = [];
                       solved = TSet.empty }
  let clear () = ()
                   
end

module type API = sig
  type assign
  type sassign
  val init: (sign,assign,sassign) slot_machine
  val clear: unit -> unit
end

type ('t,'v,'a) api = (module API with type assign = 'a
                                   and type sassign = ('t,'v)sassign)

let make (type t)(type v)(type a)
      ((module DS): (ts,values,t,v,a) dsProj)
    : (t,v,a) api =
  (module Make(DS))

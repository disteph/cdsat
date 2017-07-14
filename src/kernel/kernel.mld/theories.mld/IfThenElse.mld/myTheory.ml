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

  type termdata = Term.datatype
  type value = Value.t
  type assign = Assign.t
    
  module TSet = Set.Make(Term)
  module TMap = Map.Make(Term)

  type state = {
      treated: Assign.t;
      known: bool TMap.t;
      todo: Term.t list;
      solved: TSet.t;
      wondering: TSet.t
    }

  let add ((t,v) as nl) state =
    {
      state with
      treated = Assign.add nl state.treated;
      known   =
        (match v with
         | Values.Boolean b -> TMap.add t b state.known
         | Values.NonBoolean _ -> state.known);
      todo    = t::state.todo;
    }

  type output =
    | Sat   of (sign, Assign.t*(Term.t*bool),sat) message
    | Propa of (sign, Assign.t*(Term.t*bool),straight) message
      
  let what_now state =
    let rec aux = function
      | []   -> Some(Sat(sat () state.treated)), { state with todo = [] }
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
              let eq = Term.bC (Symbols.Eq so) [t;br] in
              Some(Propa(straight () (Assign.singleton(Values.bassign c)) (eq,true))),
              { state with todo = l; solved = TSet.add t state.solved }
            else
              None,
              { state with todo = t::l;
                           wondering = TSet.add c state.wondering }

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

  let wondering state = state.wondering

  let init = { treated = Assign.empty;
               known = TMap.empty;
               todo = [];
               solved = TSet.empty;
               wondering = TSet.empty }
               
end

module type API = sig
  type termdata
  type value
  type assign
  module TSet : Set.S with type elt = termdata termF
  type state
  type output = 
    | Sat   of (sign, assign*(termdata termF*bool),sat) message
    | Propa of (sign, assign*(termdata termF*bool),straight) message

  val add: (termdata termF * value Values.t) -> state -> state
  val what_now: state -> output option * state
  val wondering: state -> TSet.t
  val init: state
end

type ('t,'v,'a) api = (module API with type termdata = 't
                                   and type value = 'v
                                   and type assign = 'a)

let make (type t)(type v)(type a)
      ((module DS): (ts,values,t,v,a) dsProj)
    : (t,v,a) api =
  (module Make(DS))

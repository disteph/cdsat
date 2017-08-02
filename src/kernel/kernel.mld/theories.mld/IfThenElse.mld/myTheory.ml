open Top
open Messages
open Specs
open Sassigns
       
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
  type tset = TSet.t
                
  module TMap = Map.Make(Term)

  type state = { treated: Assign.t;
                 known: bool TMap.t;
                 todo: Term.t list;
                 solved: TSet.t;
                 wondering: TSet.t;
                 sharing : TSet.t;
                 myvars : TSet.t }

  let add ((SAssign(t,v)) as nl) state =
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
    | Sat   of (sign,sat) Msg.t
    | Propa of (sign,straight) Msg.t
      
  let what_now state =
    let rec aux = function
      | []   -> Some(Sat(sat () state.treated ~sharing:state.sharing ~myvars:state.myvars)),
                { state with todo = [] }
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
              let eq = Term.bC (Symbols.Eq so) [t;br], Values.Boolean true in
              Some(Propa(straight () (Assign.singleton(boolassign c)) eq)),
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
               wondering = TSet.empty;
               sharing = TSet.empty;
               myvars  = TSet.empty }
               
end

module type API = sig
  type termdata
  type value
  type assign
  type tset
  type state
  type output = 
    | Sat   of (sign, assign*(termdata termF,value)bassign*tset,sat) message
    | Propa of (sign, assign*(termdata termF,value)bassign*tset,straight) message

  val add: (termdata termF, value) sassign -> state -> state
  val what_now: state -> output option * state
  val wondering: state -> tset
  val init: state
end

type ('t,'v,'a,'s) api = (module API with type termdata = 't
                                      and type value  = 'v
                                      and type assign = 'a
                                      and type tset   = 's)

let make (type t v a s)
      ((module DS): (ts,values,t,v,a,s) dsProj)
    : (t,v,a,s) api =
  (module Make(DS))

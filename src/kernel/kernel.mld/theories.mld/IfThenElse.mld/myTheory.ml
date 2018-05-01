open General
open Top
open Basic
open Messages
open Specs
open Sassigns
       
open Termstructures

type sign = unit

(* We are using VarSets as alternative term representations *)

module TS = VarSet.ITE

include Theory.HasNoValues

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
  val share: tset -> state -> state
  val what_now: state -> output option * state
  val wondering: state -> tset
  val init: state
end

type ('t,'v,'a,'s) api = (module API with type termdata = 't
                                      and type value  = 'v
                                      and type assign = 'a
                                      and type tset   = 's)

let make (type t v a s)
    ((module DS): ((t,s)TS.t,values,t,v,a,s) dsProj)
    : (t,v,a,s) api =
  (module struct

    open DS

    type termdata = Term.datatype
    type value = Value.t
    type assign = Assign.t
    type tset = TSet.t

    module TMap = struct
      include Map.Make(Term)
      let pp_binding fmt (t,b) = pp_bassign fmt (t,Values.Boolean b)
      let pp fmt t = List.pp pp_binding fmt (bindings t)
    end

    type state = { treated: Assign.t;
                   known  : bool TMap.t;
                   todo   : Term.t list;
                   solved : TSet.t;
                   wondering: TSet.t;
                   sharing: TSet.t;
                   myvars : TSet.t Lazy.t }

    let add_myvars term myvars =
      TSet.fold TSet.add (DS.proj(Terms.data term)) myvars

    let add ((SAssign(t,v)) as nl) state =
      { state with
        treated = Assign.add nl state.treated;
        known   =
          (match v with
           | Values.Boolean b -> TMap.add t b state.known
           | Values.NonBoolean _ -> state.known);
        wondering =
          (match v with
           | Values.Boolean _ when TSet.mem t state.wondering
             -> TSet.remove t state.wondering
           | _ -> state.wondering);
        todo    = t::state.todo;
        myvars  = lazy(add_myvars t (Lazy.force state.myvars)) }

    type output =
      | Sat   of (sign,sat) Msg.t
      | Propa of (sign,straight) Msg.t

    let what_now state =
      let rec aux l =
        Print.print ["kernel.ITE",2] (fun p -> p "kernel.ITE: looping in what_now");
        match l with
        | []   -> Some(Sat(sat () state.treated ~sharing:state.sharing ~myvars:state.myvars)),
                  { state with todo = [] }
        | t::l when TSet.mem t state.solved -> aux l
        | t::l ->
          match Terms.reveal t with
          | Terms.C(Symbols.ITE so,[c;b1;b2]) 
            ->
            if TMap.mem c state.known
            then
              let b = TMap.find c state.known in
              let br = if b then b1 else b2 in
              let eq = Term.bC (Symbols.Eq so) [t;br], Values.Boolean true in
              let justif = Assign.singleton(boolassign ~b c) in
              Print.print ["kernel.ITE",0] (fun p ->
                  p "kernel.ITE: %a ⊢  %a = %a" Assign.pp justif Term.pp t Term.pp br);
              Some(Propa(straight () justif eq)),
              { state with todo = l; solved = TSet.add t state.solved }
            else
              None,
              { state with todo = t::l;
                           wondering = TSet.add c state.wondering }

          | Terms.C(Symbols.Eq _,[a1;a2])
          | Terms.C(Symbols.NEq _,[a1;a2])
            -> aux (a1::a2::l)

          | _ -> aux l

      in
      Print.print ["kernel.ITE",1] (fun p ->
          p "kernel.ITE:\n treated=%a\n known=%a\n todo=%a"
            Assign.pp state.treated TMap.pp state.known (List.pp Term.pp) state.todo);
      aux state.todo

    let wondering state = state.wondering

    let share tset state =
      let sharing = TSet.union tset state.sharing in
      let myvars = lazy(TSet.fold add_myvars tset (Lazy.force state.myvars)) in
      { state with sharing; myvars }

    let init = { treated = Assign.empty;
                 known   = TMap.empty;
                 todo    = [];
                 solved  = TSet.empty;
                 wondering = TSet.empty;
                 sharing = TSet.empty;
                 myvars  = lazy TSet.empty }

  end)


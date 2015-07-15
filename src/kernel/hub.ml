(******************)
(* Theory Manager *)
(******************)

open Format

open Top
open Basic
open Interfaces_basic
open Messages

open Prop.Formulae

(*********************************************************************)
(* First, we collect, from the various available theories, the
   datatypes that they use to represent terms.

   We organise a traversal of the list of theories; the module that we
   build after seeing each theory is of the following module type:   *)
(*********************************************************************)

module type DataType = Terms.DataType with type leaf := IntSort.t

(* Base case in the traversal *)

let noTheory: (module DataType with type t = unit) =
  (module struct
    type t = unit
    let bC _ _ _ = ()
    let bV _ = ()
  end)

(* Incremental case in the traversal *)

module Semantic2DataType(Sem:Specs.Semantic) = struct
  type t = Sem.t
  let bC tag symb args = 
    match Sem.semantic symb with
    | Some f -> f args
    | None   -> let (so,_) = Symbol.arity symb in
                Sem.leaf(IntSort.buildH(tag,so))
  let bV = Sem.leaf
end

module Pairing(B1: DataType)(B2: DataType) = struct
    type t = B1.t*B2.t
    let bC tag symb args = 
      (B1.bC tag symb (List.map fst args),
       B2.bC tag symb (List.map snd args))
    let bV v = (B1.bV v, B2.bV v)
  end

let addTheory (type a)(type b)
    (th:(module Specs.Semantic with type t = a))
    (i :(module DataType with type t = b)) 
    :(module DataType with type t = a*b) =
  let module Th = (val th) in
  let module Sem = Semantic2DataType(Th) in
  let module I = (val i) in
  (module Pairing(Sem)(I))


type _ dataList =
| NoData : unit dataList
| ConsData: (module Specs.Semantic with type t = 'a) * 'b dataList -> ('a*'b) dataList

let rec make_datastruct:
type b. b dataList -> (module DataType with type t = b) = 
  function
  | NoData -> noTheory
  | ConsData(th,l') -> (addTheory th (make_datastruct l'))

module Handlers: Map.OrderedType = struct
  type t = Handler: 'a Register.t -> t
  let id _ = 0
  let compare a b = Pervasives.compare (id a) (id b)
end

module HandlersMap = Map.Make(Handlers)
    
(* Now the initialisation of the theory manager, calls the above
   traversal function, and converts its result into the real module that
   we want, of the following module type *)
    
let make (type a)(type b) 
    (propds:(module Specs.Semantic with type t = a))
    (theories: unit HandlersMap.t)
    (l: b dataList)
    : (module Interfaces.WhiteBoard with type DS.formulaF = a and type DS.Term.datatype = a*b) =

  (* First we do the traversal *)

  let module M = (val make_datastruct l) in
  let module PropDS = (val propds) in
  let module Sem = Semantic2DataType(PropDS) in

  (module struct

    module DS = struct

      module Builder = Pairing(Sem)(M)

      module Term = Terms.Make(IntSort)(Builder)

      type formulaF = PropDS.t
      let asF = fst

      module TSet = Prop.Sequents.MakeCollectTrusted(
        struct
          type t       = Builder.t term
          let id       = Terms.id
          let compare  = Terms.compare
          let clear    = Term.clear
          let print_in_fmt = Term.print_in_fmt
        end)

    end

    let init_state =
      HandlersMap.fold
        (fun th () -> HandlersMap.add th false)
        theories
        HandlersMap.empty

    open DS

    module type InsertCoin = sig
      type t
      val take : 'a Register.t -> ('a,TSet.t) message -> t
      (* val gimmeFreshEigen: Sorts.t -> World.FreeVar.t * t *)
    end

    type answer = Provable of TSet.t | NotProvable of TSet.t
    type output = Jackpot of answer | InsertCoin of (module InsertCoin with type t=output)

    let rec search set m =
      if HandlersMap.for_all (fun _ b -> b) m
      then Jackpot(NotProvable set)
      else Jackpot(Provable set)

    let goal_consistency t atomN =
      if TSet.mem t atomN then Provable (TSet.add t TSet.empty)
      else NotProvable atomN
        
    let consistency atomN =
      TSet.fold
        (function l -> function
        | Provable set as ans -> ans
        | _   ->
	  (match goal_consistency (Term.bC Symbol.Neg [l]) atomN with
	  | Provable set -> Provable (TSet.add l set)
	  | ans     -> ans
	  )
        )
        atomN
        (NotProvable atomN)
        
  end)

(*     module type InsertCoin = sig *)
(*       type t *)
(*       val unsat: int -> TSet.t -> t *)
(*       val sat  : int -> TSet.t -> t *)
(*       val write: int -> TSet.t -> t *)
(*       val read : int -> TSet.t*t *)
(*       val gimmeFreshEigen: Sorts.t -> World.FreeVar.t * t *)
(*     end *)




(* let rec treat world state buffer:(module InsertCoin with type t = output) = *)
(*   (module struct *)

(*     type t = output *)

(*     let unsat i tset = Jackpot (Provable tset) *)

(*     let sat i tset = *)
(*       let newstate = IntMap.add i true state in *)
(*       let happyNdone =  *)
(*         (IntMap.for_all (fun _ b -> b) newstate) *)
(*         && (IntMap.for_all (fun _ -> TSet.is_empty) buffer) *)
(*       in *)
(*       if happyNdone then Jackpot (NotProvable tset) *)
(*       else InsertCoin(treat world newstate buffer) *)

(*     let write i conclusions = *)
(*       let newbuffer = *)
(*         IntMap.map (fun buffer_i -> TSet.union conclusions buffer_i) buffer *)
(*       in *)
(*       InsertCoin(treat world state newbuffer) *)

(*     let read i = *)
(*       let newstate = IntMap.add i false state in *)
(*       let todo = IntMap.find i buffer in *)
(*       let newbuffer = *)
(*         IntMap.add i TSet.empty buffer *)
(*       in *)
(*       todo,InsertCoin(treat world newstate newbuffer) *)

(*     let gimmeFreshEigen sort = *)
(*       let neweigen, newworld = World.liftE sort world in *)
(*       neweigen,InsertCoin(treat newworld state buffer) *)

(*   end) *)

(* let newtreat world = treat world init *)


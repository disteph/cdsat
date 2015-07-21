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

module Handlers = struct
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

    let init_map =
      HandlersMap.fold
        (fun th () -> HandlersMap.add th false)
        theories
        HandlersMap.empty

    open DS

    type hubsays = HubSays of TSet.t*(TSet.t->TSet.t)

    module type InsertCoin = sig
      type t
      val thdone : 'a Register.t -> ('a,TSet.t) thdone -> t
      val thsays : 'a Register.t -> ('a,TSet.t) thsays -> hubsays*t
      (* val gimmeFreshEigen: Sorts.t -> World.FreeVar.t * t *)
    end

    type answer = Provable of TSet.t | NotProvable of TSet.t
    type output = Jackpot of answer | InsertCoin of (module InsertCoin with type t=output)

    let rec ou v1 v2 success1 success2 seq cont = 
      let newcont1 = function
	| Provable seq1    -> cont(success1 seq1)
	| NotProvable seq1 ->
	  let newcont2 = function
	    | Provable seq2  -> cont(success2 seq2)
	    | NotProvable _  -> cont(NotProvable seq)
	  in
	  v2 newcont2
      in
      v1 newcont1

    let rec et v1 v2 success seq cont =
      let newcont1 = function
	| Provable seq1 ->
	  let newcont2 = function
	    | Provable seq2 -> cont(success seq1 seq2)
	    | NotProvable _ -> cont(NotProvable seq)
	  in
	  v2 newcont2
	| NotProvable _     -> cont(NotProvable seq)
      in
      v1 newcont1

    let rec straight v success seq cont =
      let newcont = function
	| Provable seqrec -> cont(success seqrec)
	| NotProvable _   -> cont(NotProvable seq)
      in
      v newcont


    let rec search set m cont :(module InsertCoin with type t=output)
        = (module struct

          type t = output

          let thdone hdl = function
            | ThProvable(_,newset)    -> cont (Jackpot(Provable newset))
            | ThNotProvable(_,newset) ->
              if TSet.subset set newset then 
                let newmap = HandlersMap.add (Handlers.Handler hdl) true m in
                if HandlersMap.for_all (fun _ b -> b) newmap
                then cont(Jackpot(NotProvable set))
                else InsertCoin(search set newmap cont)
              else failwith "ThPlugin is trying to cheat" 

          let thsays hdl = function
            | ThStraight(_,newset,justif)     -> HubSays(newset,justif),InsertCoin(search (TSet.union set newset) init_map cont)

            | ThAnd(_,newset1,newset2,justif) -> failwith "Not Implemented" 

            | ThOr(_,newset1,newset2,justif)  -> failwith "Not Implemented"

        end)
        
    let consistency atomN = InsertCoin(search atomN init_map (fun a->a))

    let goal_consistency t atomN = InsertCoin(search (TSet.add (Term.bC Symbol.Neg [t]) atomN) init_map (fun a->a))
        
  end)


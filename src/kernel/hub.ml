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
  let id (Handler hdl) = Register.id hdl
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

    open DS

    type answer = Provable of TSet.t | NotProvable of TSet.t

    type _ thanswer = ThAns : 'a Register.t * ('a,TSet.t,'b) thsays -> 'b thanswer

    type planswer = 
    | PlProvable    : thProvable thanswer -> planswer
    | PlNotProvable : TSet.t*(thNotProvable thanswer list) -> planswer

    let check = function
      | PlProvable(ThAns(_,ThProvable thset)) -> Provable thset
      | PlNotProvable(newset,l) -> 
        if HandlersMap.is_empty
          (List.fold_right
             (fun (ThAns(hdl,ThNotProvable thset)) thok -> 
               if TSet.equal newset thset
               then HandlersMap.remove (Handlers.Handler hdl) thok
               else failwith "Theories disagree on model")
             l
             theories)
        then NotProvable newset
        else failwith "Not all theories have stamped the model"
              
  end)


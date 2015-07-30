(*********************)
(* Theory Combinator *)
(*********************)

open Format

open Top
open Basic
open Interfaces_basic
open Messages
open Theories_register

open Prop.Formulae

(* This is the module type that we are going to produce at the end of this file *)

module type WhiteBoard = sig
  module DS : Top.Specs.GTheoryDSType
  open DS
  type answer = private Provable of TSet.t | NotProvable of TSet.t
  type planswer = 
  | PlProvable    : (TSet.t,thProvable) thanswer -> planswer
  | PlNotProvable : TSet.t*((TSet.t,thNotProvable) thanswer list) -> planswer
  val check : planswer -> answer
end

(*********************************************************************)
(* First, we build DS by aggregating a given list of plugins'
   datatypes for representing terms, into one big datatype.

   What we call "a plugin's datatype" is given by the module type
   Top.Specs.Semantic
   in which some symbols might not have any interpretation for the
   plugin. 

   We shall quickly convert them in the following module type
   DataType
   where all symbols and all terms can be represented *)
(*********************************************************************)

module type DataType = Terms.DataType with type leaf := IntSort.t

module Semantic2DataType(Sem:Specs.Semantic) = struct
  type t = Sem.t
  let bC tag symb args = 
    match Sem.semantic symb with
    | Some f -> f args
    | None   -> let (so,_) = Symbol.arity symb in
                Sem.leaf(IntSort.buildH(tag,so))
  let bV = Sem.leaf
end

(* Now we shall organise a traversal of a given list of
   Top.Specs.Semantic modules, aggregated all of the datatypes into
   one *)

(* Base case in the traversal *)

let noTheory: (module DataType with type t = unit) =
  (module struct
    type t = unit
    let bC _ _ _ = ()
    let bV _ = ()
  end)

(* Incremental case in the traversal *)

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

(* Now, this is what we mean by "a list of Top.Specs.Semantic
   modules": it's a list, except it's indexed by the aggregated type
   itself, as a product: *)

type _ dataList =
| NoData : unit dataList
| ConsData: (module Specs.Semantic with type t = 'a) * 'b dataList -> ('a*'b) dataList

(* Now, we shall be given a list of the above form, which we shall
   aggregate into datatype, but we shall also have to provide a list
   of projections from the aggregated datatype into each plugin's
   datatype. That list of projections (of the same length of the input
   list) is again an indexed list, of the type below: *)

type (_,_) projList =
| NoProj : (_,unit) projList
| ConsProj: ('t -> 'a) * ('t,'b) projList -> ('t,'a*'b) projList

(* Now we finally organise the traversal: *)

let rec make_datastruct:
type a b. a dataList -> (module DataType with type t = a) * ((b -> a) -> (b,a) projList) 
  = function
  | NoData          -> noTheory, fun _ -> NoProj
  | ConsData(th,l') -> let i, make_pl = make_datastruct l' in
                       addTheory th i, fun f -> ConsProj((fun x -> fst(f x)),make_pl (fun x -> snd(f x)))

(* Now comes the initialisation of the theory combinator, taking as
   input a set of theory handlers and a list of plugins' datatypes. It
   calls the above traversal function, and then produces a
   "whiteboard" (first module type in this file) together with the
   aforementioned projections *)
    
let make (type a)
    (theories: unit HandlersMap.t)
    (l: a dataList)
    : (module WhiteBoard with type DS.Term.datatype = a)
    * (a,a) projList
    =

  (* First we do the traversal *)

  let dt,pl = make_datastruct l in
  let module DT = (val dt) in

  (module struct

    module DS = struct

      module Term = Terms.Make(IntSort)(DT)

      module TSet = Prop.Sequents.MakeCollectTrusted(
        struct
          type t       = DT.t term
          let id       = Terms.id
          let compare  = Terms.compare
          let clear    = Term.clear
          let print_in_fmt = Term.print_in_fmt
        end)

    end

    open DS

    type answer = Provable of TSet.t | NotProvable of TSet.t

    type planswer = 
    | PlProvable    : (TSet.t,thProvable) thanswer -> planswer
    | PlNotProvable : TSet.t*((TSet.t,thNotProvable) thanswer list) -> planswer

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
              
  end),
  pl (fun x -> x)


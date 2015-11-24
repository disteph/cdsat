(*********************)
(* Theory Combinator *)
(*********************)

open Format

open Top
open Basic
open Interfaces_basic
open Variables
open Messages
open Theories_register
open Specs

(* This is the module type that we are going to produce at the end of this file *)

module type WhiteBoard = sig
  module DS : GTheoryDSType
  open DS
  type answer = private
                | Provable of unit HandlersMap.t*TSet.t
                | NotProvable of unit HandlersMap.t*TSet.t
  val provable    : 'a Sig.t -> ('a,TSet.t,thProvable) thsays -> answer
  val notprovable_init: TSet.t -> answer
  val notprovable : 'a Sig.t -> ('a,TSet.t,thNotProvable) thsays -> answer -> answer
  val straight: 'a Sig.t -> ('a,TSet.t,thStraight) thsays -> answer -> answer
  val andBranch: 'a Sig.t -> ('a,TSet.t,thAnd) thsays -> answer -> answer -> answer
  val orBranch: 'a Sig.t -> ('a,TSet.t,thOr) thsays -> bool -> answer -> answer
end

(*********************************************************************)
(* First, we build DS by aggregating a given list of plugins'
   datatypes for representing terms, into one big datatype.

   What we call "a plugin's datatype" is given by the module type
   Top.Specs.DataType
   in which some symbols might not have any interpretation for the
   plugin. 

   We shall quickly convert them in the following module type
   DataType
   where all symbols and all terms can be represented *)
(*********************************************************************)


(* Now we shall organise a traversal of a given list of
   Top.Specs.DataType modules, aggregated all of the datatypes into
   one *)

(* Base case in the traversal *)

let noTheory: (module DataType with type t = unit) =
  (module struct
    type t = unit
    let bC _ _ _ = ()
    let bV _ _   = ()
  end)

(* Incremental case in the traversal *)

let addTheory (type a)(type b)
    (th:(module DataType with type t = a))
    (i :(module DataType with type t = b)) 
    :(module DataType with type t = a*b) =
  let module Th = (val th) in
  let module I = (val i) in
  (module Pairing(Th)(I))

(* Now, this is what we mean by "a list of Top.Specs.DataType
   modules": it's a list, except it's indexed by the aggregated type
   itself, as a product: *)

type _ dataList =
| NoData : unit dataList
| ConsData: (module DataType with type t = 'a) * 'b dataList -> ('a*'b) dataList

(* Now, we shall be given a list of the above form, which we shall
   aggregate into datatype, but we shall also have to provide a list
   of projections from the aggregated datatype into each plugin's
   datatype. That list of projections (of the same length of the input
   list) is again an indexed list, of the type below: *)

type (_,_) projList =
| NoProj  : (_,unit) projList
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

      module Term = Terms.Make(FreeVar)(DT)

      module TSet = MakeCollection(
        struct
          type t       = DT.t termF
          let id       = Terms.id
          let compare  = Terms.compare
          let clear    = Term.clear
          let print_in_fmt = Term.print_in_fmt
        end)

    end
      
    open DS
    type answer =
      | Provable of unit HandlersMap.t*TSet.t
      | NotProvable of unit HandlersMap.t*TSet.t

    let check hdl = if HandlersMap.mem (Handlers.Handler hdl) theories then ()
      else failwith "Using a theory that is not allowed"

    let provable hdl (ThProvable thset) =
      check hdl;
      Provable(HandlersMap.add (Handlers.Handler hdl) () HandlersMap.empty,
               thset)

    let notprovable_init tset = NotProvable(theories,tset)

    let notprovable hdl (ThNotProvable newtset) = function
      | NotProvable(rest,tset) ->
         if TSet.equal newtset tset
         then NotProvable(HandlersMap.remove (Handlers.Handler hdl) rest,
                          tset)
         else failwith "Theories disagree on model"
      | _ -> failwith "Should apply WB.notprovable on NotProvable"

    let straight hdl (ThStraight(newset,oldset)) = function
      | Provable(hdls,thset) ->
         check hdl;
        Provable(HandlersMap.add (Handlers.Handler hdl) () hdls,
                 TSet.union (TSet.diff thset newset) oldset)
      | NotProvable _ -> failwith "Should apply WB.straight on Provable"

    let andBranch hdl (ThAnd(new1,new2,oldset)) ans1 ans2 =
      match ans1,ans2 with 
      | Provable(hdls1,thset1), Provable(hdls2,thset2) ->
         check hdl;
        Provable(HandlersMap.add (Handlers.Handler hdl) () 
                   (HandlersMap.merge (fun _ _ _ -> Some()) hdls1 hdls2),
                 TSet.union (TSet.diff thset1 new1) 
                   (TSet.union (TSet.diff thset2 new2) oldset))
      | _ -> failwith "Should apply WB.andBranch on two Provable"

    let orBranch hdl (ThOr(new1,new2,oldset)) side = function
      | Provable(hdls,thset) ->
         check hdl;
        let newset = if side then new1 else new2 in
        Provable(HandlersMap.add (Handlers.Handler hdl) () hdls,
                 TSet.union (TSet.diff thset newset) oldset)
      | NotProvable _ -> failwith "Should apply WB.orBranch on Provable"
         
  end),
  pl (fun x -> x)


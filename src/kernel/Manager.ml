(******************)
(* Theory Manager *)
(******************)

open Format
open Lwt

open Kernel
open Basic
open Interfaces_basic
open Formulae

open Theories_tools.StandardStruct
open OpenModule

(*********************************************************************)
(* First, we collect, from the various available theories, the
   datatypes that they use to represent terms.

   We organise a traversal of the list of theories; the module that we
   build after seeing each theory is of the following module type:   *)
(*********************************************************************)

module type Intermediary = sig

  module Builder : Semantic with type leaf := IntSort.t

  val make
    : ('a -> Builder.t)
    -> (module TermType with type datatype = 'a)
    -> (module CollectTrusted with type e = 'a term and type t = 'b)
    -> ('a,'b) OM.resume list
end

(* Base case in the traversal *)

let noTheory: (module Intermediary) = 
  (module struct

    module Builder = struct
      type t = unit
      let semantic _ _ _ = ()
      let leaf _ = ()
    end 

    let make _ _ _ = []

  end)

(* Incremental case in the traversal *)

let addTheory (i:(module Intermediary)) (th:(module OM.Type))
    :(module Intermediary) =
  let module Th = (val th) in
  let module I = (val i) in
  (module struct

    module Builder = struct
      type t = I.Builder.t*Th.Builder.t
      let semantic tag symb args = 
        (I.Builder.semantic tag symb (List.map fst args),
         match Th.Builder.semantic symb with
         | Some f -> f (List.map snd args)
         | None   -> let (so,_) = Symbol.arity symb in
                     Th.Builder.leaf (IntSort.buildH (tag,so))
        )
      let leaf v = (I.Builder.leaf v, Th.Builder.leaf v)
    end 

    let make (type a)(type b)
        (p: a-> Builder.t)
        (terms: (module TermType with type datatype = a))
        (tset : (module CollectTrusted with type e = a term and type t = b))
        =
      (Th.make (fun a -> snd(p a)) terms tset)
      ::(I.make (fun a -> fst(p a)) terms tset)

  end)

(* Now the initialisation of the theory manager, calls the above
traversal function, and converts its result into the real module that
we want, of the following module type *)

let init (l:(module OM.Type) list)
    : (module Theories_tools.ForGround.Type) =

  (* First we do the traversal *)

  let module M = (val List.fold_left addTheory noTheory l) in

  (module struct

    let names    = []
    let sugPlugin= None

    include StandardDSData(IntSort)(M.Builder)

    module ForParsing = ForParsingWOEx
    module Terms = Atom.Term

    module TSet = Sequents.MakeCollectTrusted(
      struct
        type t    = M.Builder.t term
        let id t  = TermDef.id t
        let clear = Terms.clear
        let compare a b  = Pervasives.compare (id a) (id b)
        let print_in_fmt = Terms.print_in_fmt
      end)

    let theories: (Terms.datatype,TSet.t) OM.resume list = 
      let terms = (module Terms : TermType with type datatype = M.Builder.t) in
      let tset  = (module TSet  : CollectTrusted with type e = M.Builder.t term and type t = TSet.t) in
      M.make (fun a -> a) terms tset

    let nb_th, init_state, init_buffer =
      List.fold_left 
        (fun (i, state, buffer) th 
        -> (i+1,IntMap.add i false state,IntMap.add i [] buffer))
        (0,IntMap.empty,IntMap.empty)
        theories


    module type InsertCoin = sig
      type t
      val unsat: int -> TSet.t -> t
      val sat  : int -> TSet.t -> t
      val write: int -> TSet.t -> t
      val read : int -> TSet.t*t
      val gimmeFreshEigen: Kernel.Sorts.t -> Kernel.World.FreeVar.t * t
    end

    type answer = Provable of TSet.t | NotProvable
    type output = Jackpot of answer | InsertCoin of (module InsertCoin with type t = output)

    let rec treat world state buffer:(module InsertCoin with type t = output) =
      (module struct

        type t = output

        let unsat i tset = Jackpot (Provable tset)

        let sat i tset =
          let newstate = IntMap.add i true state in
          let happyNdone = 
            (IntMap.for_all (fun _ b -> b) newstate)
            && (IntMap.for_all (fun _ -> TSet.is_empty) buffer)
          in
          if happyNdone then Jackpot NotProvable
          else InsertCoin(treat world newstate buffer)

        let write i conclusions =
          let newbuffer =
            IntMap.map (fun buffer_i -> TSet.union conclusions buffer_i) buffer
          in
          InsertCoin(treat world state newbuffer)

        let read i =
          let newstate = IntMap.add i false state in
          let todo = IntMap.find i buffer in
          let newbuffer =
            IntMap.add i TSet.empty buffer
          in
          todo,InsertCoin(treat world newstate newbuffer)

        let gimmeFreshEigen sort =
          let neweigen, newworld = World.liftE sort world in
          neweigen,InsertCoin(treat newworld state buffer)

      end)

    (* let newtreat world = treat world init *)

    module GConsistency(EAtom : sig
      type t
      val proj: t -> Atom.t
      val negation: t->t
    end) = struct

      module ASet = Sequents.MakeCollectTrusted(
        struct
          type t    = EAtom.t
          let id t  = Atom.id(EAtom.proj t)
          let clear = Atom.clear
          let compare a b = Atom.compare (EAtom.proj a) (EAtom.proj b)
          let print_in_fmt fmt a = Atom.print_in_fmt fmt (EAtom.proj a)
        end)

      let goal_consistency t atomN = 
        if ASet.mem t atomN then Some (ASet.add t ASet.empty)
        else None
          
      let rec consistency atomN =
        ASet.fold 
          (function l -> function
          | Some a -> Some a
          | None   -> 
	    (match goal_consistency (EAtom.negation l) atomN with
	    | None     -> None
	    | Some set -> Some (ASet.add l set)
	    )
          )
          atomN
          None
    end


  end)

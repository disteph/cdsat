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

module type Intermediary = sig

  module Builder : Terms.DataType with type leaf := IntSort.t

  val make
    : ('a -> Builder.t)
    -> (module Specs.Term with type datatype = 'a)
    -> (module CollectTrusted with type e = 'a term and type t = 'b)
    -> ('a,'b) resume list

end

(* Base case in the traversal *)

let noTheory: (module Intermediary) =
  (module struct

    module Builder = struct
      type t = unit
      let bC _ _ _ = ()
      let bV _ = ()
    end

    let make _ _ _ = []

  end)

(* Incremental case in the traversal *)

module Semantic2DataType(Sem:Specs.Semantic)
  :Terms.DataType with type leaf := IntSort.t
                  and  type t = Sem.t           =
struct
  type t = Sem.t
  let bC tag symb args = 
    match Sem.semantic symb with
    | Some f -> f args
    | None   -> let (so,_) = Symbol.arity symb in
                Sem.leaf(IntSort.buildH(tag,so))
  let bV = Sem.leaf
end

let addTheory (i:(module Intermediary)) (th:(module Specs.Theory))
    :(module Intermediary) =
  let module Th = (val th) in
  let module Sem = Semantic2DataType(Th.Semantic) in
  let module I = (val i) in
  (module struct

    module Builder = struct
      type t = I.Builder.t*Sem.t
      let bC tag symb args = 
        (I.Builder.bC tag symb (List.map fst args),
         Sem.bC tag symb (List.map snd args))
      let bV v = (I.Builder.bV v, Sem.bV v)
    end 

    let make (type a)(type b)
        (p: a-> Builder.t)
        (terms: (module Specs.Term with type datatype = a))
        (tset : (module CollectTrusted with type e = a term and type t = b))
        =
      (Th.make (fun a -> snd(p a)) terms tset)
      ::(I.make (fun a -> fst(p a)) terms tset)

  end)

(* Now the initialisation of the theory manager, calls the above
traversal function, and converts its result into the real module that
we want, of the following module type *)

let make 
    (type a) 
    (propds:(module Specs.Semantic with type t = a))
    (l:(module Specs.Theory) list)
    : (module ForGround.GDecProc with type DS.formulaF = a) =

  (* First we do the traversal *)

  let module M = (val List.fold_left addTheory noTheory l) in
  let module PropDS = (val propds) in
  let module Sem = Semantic2DataType(PropDS) in

  (module struct

    module DS = struct

      module Builder = struct
        type t = M.Builder.t*Sem.t
        let bC tag symb args = 
          (M.Builder.bC tag symb (List.map fst args),
           Sem.bC tag symb (List.map snd args))
        let bV v = (M.Builder.bV v, Sem.bV v)
      end

      module Term = Terms.Make(IntSort)(Builder)

      type formulaF = PropDS.t
      let asF = snd

      module TSet = Prop.Sequents.MakeCollectTrusted(
        struct
          type t       = Builder.t term
          let id       = Terms.id
          let compare  = Terms.compare
          let clear    = Term.clear
          let print_in_fmt = Term.print_in_fmt
        end)

      let theories: (Term.datatype,TSet.t) resume list = 
        let terms = (module Term  : Specs.Term with type datatype = Builder.t) in
        let tset  = (module TSet  : CollectTrusted with type e = Builder.t term and type t = TSet.t) in
        M.make fst terms tset

      let nb_th, init_state, init_buffer =
        List.fold_left 
          (fun (i, state, buffer) th 
          -> (i+1,IntMap.add i false state,IntMap.add i [] buffer))
          (0,IntMap.empty,IntMap.empty)
          theories

    end

    open DS



    type answer = Provable of TSet.t | NotProvable of TSet.t

    (* Generator of local answer types, either genuine answer or a fake answer *)
    type ('a,'b) local = Genuine of 'a | Fake  of 'b

    (* 'a intern is the type of local answers, for internal use during
       search 

       In both cases,

       1st argument says whether it is
       - a genuine answer, with appropriate data
       - or a fake one, with boolean b saying whether we go to the
         right (b=true) or to the left (b=false)

       Last argument is the computation at resume point; 
       (it suffices to apply it to a continuation to trigger it)

       In case of success (genuine or fake), we also produce the
    constraint produced so far.
    *)

    type 'a intern =
    | Success of (TSet.t,bool) local * 'a computations
    | Fail    of (TSet.t,bool) local * 'a computations
    and 'a computations = bool -> ('a intern -> 'a) -> 'a

    module type InsertCoin = sig
      type t
      val unsat: int -> TSet.t -> t
      val sat  : int -> TSet.t -> t
      val write: int -> TSet.t -> t
      val read : int -> TSet.t*t
      val gimmeFreshEigen: Sorts.t -> World.FreeVar.t * t
    end

    module type NewInsertCoin = sig
      type t
      val take : 'a Register.t -> ('a,TSet.t) message -> t
      val gimmeFreshEigen: Sorts.t -> World.FreeVar.t * t
    end


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
          if happyNdone then Jackpot (NotProvable tset)
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


    let goal_consistency t atomN = 
      if TSet.mem t atomN then Some (TSet.add t TSet.empty)
      else None
        
    let rec consistency atomN =
      TSet.fold 
        (function l -> function
        | Some a -> Some a
        | None   -> 
	  (match goal_consistency (Term.bC Symbol.Neg [l]) atomN with
	  | None     -> None
	  | Some set -> Some (TSet.add l set)
	  )
        )
        atomN
        None


  end)

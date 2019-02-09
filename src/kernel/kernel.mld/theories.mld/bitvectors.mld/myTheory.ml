open Format
       
open General
open Monads

open Top
open Messages
open Terms
open Values
open Sassigns

open Theory
    
type sign = unit

module V = Bv_value
  
(* We are using the above as values *)
let vkey  = Values.Key.make(module V)
let vinj  = Value.inj vkey
let vproj = Value.proj vkey

(* We are using VarSets (sets of variables) as alternative term representations *)
module TS = Termstructures.VarSet.BV

type valuation = Eq.MyTheory.sign Eq.Valuation.signed

module type API = sig
  type state
  val init: state
  (* val term_eval : (Term.t -> V.t) -> Term.t -> V.t
   * val form_eval : (Term.t -> V.t) -> Term.t -> bool *)
  val eval : valuation -> Term.t -> (sign, straight) message * int
end

module T = struct
  let dskey = Termstructures.VarSet.Arrays.key
  let ds  = [DSK dskey]
  type nonrec sign = sign
  type api = (module API)
  let name = "BV"
  let proj  = Terms.proj dskey

  module Make(W : Writable) (* : API *) = struct

    (* First, we implement evaluation functions for bitvector terms and predicates *)

    (* Exception that we raise if we cannot evaluate a bitvector formula *)
    exception CannotEval of string

    module Eval(M: sig
        include Monad
        val var : (CValue.t * (Assign.t*int) Lazy.t) -> V.t t
      end) = struct

      include Monads.Make_Let(M)

      (* evaluation of a term *)
      (*TODO: Semantics to be checked!!!*)
      
      let term_eval valuation term =
        let open Symbols in
        let open Terms in
        let valuation = Eq.Valuation.reveal valuation in
        let rec aux term =
          match Term.reveal term with
          | C(BVextract{hi;lo},[a]) -> let%map a = aux a in V.select_e a hi lo
          | C(BVconc _,[a;b]) -> let%map a = aux a and b = aux b in V.(a @: b)
          | C(BVcst v,[])     -> M.return v
          | C(BVnot v,[a])    -> let%map a = aux a in V.( ~: a)
          | C(BVneg v,[a])    -> let%map a = aux a in V.negate a
          | C(BVand _,[a;b])  -> let%map a = aux a and b = aux b in V.(a &&: b)
          | C(BVor  _,[a;b])  -> let%map a = aux a and b = aux b in V.(a ||: b)
          | C(BVxor _,[a;b])  -> let%map a = aux a and b = aux b in V.(a ^: b)
          | C(BVadd _,[a;b])  -> let%map a = aux a and b = aux b in V.(a +: b)
          | C(BVmul _,[a;b])  -> let%map a = aux a and b = aux b in V.(a *: b)
          | _ -> Eq.Valuation.find term valuation |> M.var
        in aux term

      (* evaluation of an atomic predicate *)
      let form_eval valuation ?(truthvalue=true) form =
        let open Symbols in
        let open Terms in
        let term_eval = term_eval valuation in
        let result b v = if b then V.isT v else not(V.isT v) in
        match Term.reveal form with

        | Terms.C(Symbols.Eq(Sorts.BV _),[a;b])  ->
          let%map a = term_eval a and b = term_eval b in
          result truthvalue V.(a ==: b)

        | Terms.C(Symbols.NEq(Sorts.BV _),[a;b]) ->
          let%map a = term_eval a and b = term_eval b in
          result (not truthvalue) V.(a ==: b)

        | Terms.C(Symbols.BVult _,[a;b]) ->
          let%map a = term_eval a and b = term_eval b in
          result truthvalue V.(a <: b)

        | _ -> raise(CannotEval(Format.stringOf Term.pp form))

    end

    module Simpl = struct
      module M = struct
        include IdMon
        let var (v,_) =
          match CValue.proj vkey v with
          | Some(Values(NonBoolean v)) -> v
          | _ -> failwith "Egraph lied"
      end
      include Eval(M)
    end

    module Msg = struct
      module M = struct
        include StateMonad(struct type t = Assign.t*int end)
        let var (v,l) (assign0,i0) =
          match CValue.proj vkey v with
          | Some(Values(NonBoolean v)) ->
            let assign, i = Lazy.force l in
            v, (Assign.union assign assign0, max i i0)
          | _ -> failwith "Egraph lied"
      end
      include Eval(M)
    end

    (* Evaluation function for a formula, given a valuation.
       Either raises an exception CannotEval,
       or produces a message Propa(assign,boolassign), where
       boolassign is the Boolean assignment saying if the formula evaluates to true or false
       assign are the assignments that were used in the evaluation *)
    let eval valuation term =
      match Term.get_sort term with
      | Sorts.Prop ->
        let b,(assign,i) = Msg.form_eval valuation term (Assign.empty,0) in
        straight () assign (term,Values.Boolean b), i
      | s -> raise (CannotEval(Format.toString(fun p ->
          p "I do not know sort %a" Sorts.pp s)))

    (* States of the algorithm *)
    type state = { seen      : Assign.t;
                   sharing   : TSet.t;
                   myvars    : TSet.t Lazy.t;
                   constraints : SAssign.t list }

    (* Initial state *)
    let init = { seen      = Assign.empty;
                 sharing   = TSet.empty;
                 myvars    = lazy TSet.empty;
                 constraints = [] }


    let sat valuation state =
      let rec aux = function
        | [] -> { state with constraints=[] },
                Some(sat () state.seen ~sharing:state.sharing ~myvars:state.myvars)
        | c::constraints ->
          
    
  end

  let make (module W : Writable) : api = (module Make(W))

end

let hdl = register(module T)

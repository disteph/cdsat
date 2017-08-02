open Format
       
open General
open Top
open Basic
open Messages
open Specs
open Sassigns

type sign = unit

module CstBV = HardCaml.Bits.Comb.ArraybitsNativeint

              
module V = struct
  include CstBV

  let isT a = [%eq:nativeint array * int] (a ==: vdd) vdd
  let equal a b = isT(a ==: b)
  let compare a b = if equal a b then 0
                    else if isT(a <=: b) then 1 else -1
  let hash a = CstBV.to_int(CstBV.select a 16 0)
  let hash_fold_t = Hash.hash2fold hash
  let pp fmt a =
    let rec aux fmt = function
      | [] -> ()
      | t::q -> fprintf fmt "%s%a" (if t then "1" else "0") aux q
    in
    aux fmt (List.map isT (CstBV.bits a))
  let show = Print.stringOf pp
end
              
(* We are using the above as values *)
include Theory.HasValues(V)

(* We are using VarSets as alternative term representations *)
type ts = Termstructures.VarSet.Generic.IntSortSet.t
let ts = Termstructures.Register.BV

module Make(DS: DSproj with type values = values
                        and type ts = ts) = struct

  open DS
  type termdata = Term.datatype
  type assign = Assign.t
  type tset = TSet.t

  let HasVconv(vinj, vproj) = conv
                
  exception CannotEval of string

  type 'a eval = {
      cst : 'a;
      sgl : Term.t -> V.t -> 'a;
      uni : 'a -> 'a -> 'a
    }
                            
  let term_eval_by f by term =
    let open Symbols in
    let rec aux term =
      match Terms.reveal term with
      | Terms.C(Extract(hi,lo,_),[a]) ->
         let a,a' = aux a in
         V.select_e a hi lo, a'
      | Terms.C(Conc _,[a;b]) ->
         let a,a' = aux a in
         let b,b' = aux b in
         V.(a @: b), by.uni a' b'
      | Terms.C(CstBV s,[]) ->
         V.constb s, by.cst
      | _ -> let v = f(Term.id term) in
             v, by.sgl term v
    in aux term

  let form_eval_by f by form =
    let open Symbols in
    match Terms.reveal form with
    | Terms.C(Eq(Sorts.BV _),[a;b]) ->
       let a,a' = term_eval_by f by a in
       let b,b' = term_eval_by f by b in
       V.equal a b, by.uni a' b'
    | Terms.C(NEq(Sorts.BV _),[a;b]) ->
       let a,a' = term_eval_by f by a in
       let b,b' = term_eval_by f by b in
       not(V.equal a b), by.uni a' b'
    | _ -> raise(CannotEval(Print.stringOf Term.pp form))

  let noby = { cst = ();
               sgl = (fun _ _ -> ());
               uni = fun () () -> () }

  let term_eval f term = fst(term_eval_by f noby term)
  let form_eval f term = fst(form_eval_by f noby term)

  let by = { cst = Assign.empty;
             sgl = (fun term v ->
               Assign.singleton(SAssign(term, Values.NonBoolean(vinj v))));
             uni = Assign.union }

  type state = { seen : Assign.t;
                 valuation : V.t IntMap.t;
                 constraints : Term.t list }

  let eval state term =
    let f i = 
      if IntMap.mem i state.valuation then IntMap.find i state.valuation
      else raise (CannotEval(string_of_int i))
    in
    match Term.get_sort term with
    | Sorts.Prop ->
       let b,assign = form_eval_by f by term
       in straight () assign (term,Values.Boolean b)
    (* | Sorts.BV _ -> *)
    (*    let v,assign = term_eval_by f by term *)
    (*    in Values.NonBoolean v, assign  *)
    | s -> raise (CannotEval(Print.toString(fun p ->
                                 p "I do not know sort %a" Sorts.pp s)))

  let init = { seen = Assign.empty;
               valuation = IntMap.empty;
               constraints = [] }
    
end

module type API = sig
  type termdata
  type assign
  type tset
  type state
  val init: state
  val term_eval : (int -> V.t) -> termdata termF -> V.t
  val form_eval : (int -> V.t) -> termdata termF -> bool
  val eval : state -> termdata termF
             -> (sign, assign * (termdata termF, _)bassign*tset, straight) message
end

type ('t,'v,'a,'s) api = (module API with type termdata = 't
                                      and type assign = 'a
                                      and type tset   = 's)

let make (type t v a s)
      ((module DS): (ts,values,t,v,a,s) dsProj)
    : (t,v,a,s) api =
  (module Make(DS))

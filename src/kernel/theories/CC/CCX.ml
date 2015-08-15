(*****************************)
(* The functor X => CC(X)    *)
(* producing a ground theory *)
(*****************************)

open Top
open Interfaces_basic
open Messages
open Specs

open Prop
open Literals

open Interfaces
open CCX_algo

type sign = unit

module Make
  (DS: sig 
    include GTheoryDSType
    val proj: Term.datatype -> LitF.t
  end)
  (X : SolvableTheory with type t = DS.Term.t)
  (U : PersistentUnionFind with type e = X.v 
                           and  type d = X.t input) 
  = 
struct

  open DS

  module Alg = Algo (X) (U)

  let goal_consistency atomN t =
    if TSet.mem t atomN then Some (TSet.add t TSet.empty)
    else
      begin
        Dump.msg (Some(fun p->p "Procedure called on goal %a under hypotheses\n%a" Term.print_in_fmt t TSet.print_in_fmt atomN)) None None;
	let l = TSet.fold (fun t l -> t::l) atomN [] in
	let l' = List.fold_left 
          (fun li e ->
	    let b,at = X.predicate e in
            match X.root at with
            | Some(Symbols.Eq _)  -> e::li
            | Some(Symbols.NEq _) -> li
            | _ -> failwith "CC(X) can only understand = and <> as predicate symbols")
          []
          l
        in
	let phi = List.map X.atoI l' in
        let at = X.itoA(X.atoI t) in
	let s = X.directSubterms at in
        let result = 
          match X.root at with
          | Some(Symbols.Eq _) -> 
            begin
	      let q = Some([Alg.toQuery (X.atoI t)]) in
	      match Alg.solve phi q with
	      | None   -> None
	      | Some l -> (*Alg.print_inputlist l; print_newline();*)
	        let li = List.map X.itoA l in
	        Some(List.fold_left (fun e a -> TSet.add a e) TSet.empty li)
            end
          | Some(Symbols.NEq _) ->
            begin
	      let r = ref true and ans = ref [] and li = ref (TSet.fold (fun i l ->
                let at' = X.itoA(X.atoI i) in
                match X.root i with
                | Some(Symbols.Eq _) -> at'::l
                | _ -> l) atomN []) in
	      while !r && !li<>[] do
	        let s' = X.directSubterms(List.hd !li) in
	        let q = Some(List.map2
                               (fun a a' -> Alg.toQuery (Eq(a,a')))
                               s s') in
	        match Alg.solve phi q with
	        | None   -> li := List.tl !li
	        | Some l -> r:=false; ans := l
	      done;
	      if !r then None else
	        let lis = (List.map X.itoA !ans) in
	        Some(List.fold_left (fun e a -> TSet.add a e) TSet.empty ((List.hd !li)::lis))
            end
          | _ -> failwith "CC(X) can only understand = and <> as predicate symbols"
        in
        (match result with 
        | None   -> Dump.msg (Some(fun p->p "Procedure finished with CONSISTENT with hypotheses.")) None None
        | Some x -> Dump.msg (Some(fun p->p "Procedure finished with INCONSISTENT with hypotheses\n %a" TSet.print_in_fmt x)) None None); 
        result
      end

  let consistency atomN = 
    Dump.msg (Some(fun p->p "Procedure called on\n%a" TSet.print_in_fmt atomN)) None None;
    let l = TSet.fold (fun t l -> t::l) atomN [] in
    let l' = List.fold_right
      (fun e li ->
	let b,at = X.predicate e in
        match X.root at with
        | Some(Symbols.Eq _)  -> e::li
        | Some(Symbols.NEq _) -> li
        | _ -> failwith "CC(X) can only understand = and <> as predicate symbols")
      l
      []
    in
    let result = 
      if l' = l then 
        begin
	  let phi = List.map X.atoI l in
	  match Alg.solve phi None with
	  | None -> None
	  | Some l -> (*Alg.print_inputlist l; print_newline();*)
	    let li = List.map X.itoA l in
	    Some(List.fold_left (fun e a -> TSet.add a e) TSet.empty li)
        end
      else 
        begin
	  let r = ref None in
	  let phi = List.map X.atoI l' in 
          begin
	    match Alg.solve phi None with
	    | None -> ()
	    | Some l -> (*Alg.print_inputlist l; print_newline();*)
	      let li = (List.map X.itoA l) in
	      r := Some(List.fold_left (fun e a -> TSet.add a e) TSet.empty li)
	  end;
 	  if !r <> None then !r else 
            begin
	      let b = ref true and li = ref l and ans = ref TSet.empty in
	      while !b && !li<>[] do
                begin
                  match X.root(List.hd !li) with
                  | Some(Symbols.Eq _)  -> ()
                  | Some(Symbols.NEq _) -> 
                    begin
	              match goal_consistency atomN (Term.bC Symbols.Neg [List.hd !li]) with
	              | None   -> ()
	              | Some s -> b:=false ; ans:=TSet.add (List.hd !li) s
                    end
                  | _ -> failwith "CC(X) can only understand = and <> as predicate symbols"
                end;
                li := List.tl !li;
	      done;
	      if !b then None else
	        Some(TSet.add (List.hd !li) !ans)
	    end 
        end
    in
    (match result with 
    | None   -> Dump.msg (Some(fun p->p "Procedure finished with CONSISTENT with hypotheses.")) None None
    | Some x -> Dump.msg (Some(fun p->p "Procedure finished with INCONSISTENT with hypotheses\n %a" TSet.print_in_fmt x)) None None); 
    result
end

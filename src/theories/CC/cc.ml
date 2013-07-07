open Kernel
open Interfaces
open Theories
open AlgoCC
open PUFind

(* the theory CC(X) *)

module CCModulo (X : SolvableTheory) (U:PersistentUnionFind with type e = X.v 
and type d = X.input) : ThDecProc = struct
    
  module Sig = X.Sig

  module Atom = X.Atom

  let sugPlugin = None

  module Structure = X.Structure

  module Alg = Algo (X) (U)

  module Consistency(ASet : CollectImplem with type e = Atom.t) = struct
      
    let goal_consistency atomN t =
      if ASet.is_in t atomN then Some (ASet.add t ASet.empty)
      else
	let l = ASet.fold (fun t l -> t::l) atomN [] in
	let l' = (List.fold_left (fun li e ->
	  let (b,p,s) = X.predicate e in
	  if p = "=" then e::li else li) [] l) in
	let phi = (List.map X.atoI l') in
	let (b,p,s) = X.predicate t in
	if p = "=" then
	  let q = Some([Alg.toQuery (X.atoI t)]) in
	  match (Alg.solve phi q) with
	  | None -> None
	  | (Some l) -> (*Alg.print_inputlist l; print_newline();*)
	    let li = (List.map X.itoA l) in
	    Some(List.fold_left (fun e a -> 
	      ASet.add a e) ASet.empty li)
	else begin
	  let r = ref true and ans = ref [] and li = ref (ASet.fold (fun i l ->
	    let (b',p',s') = X.predicate i in
	    if b'=b && p'=p then i::l else l) atomN []) in
	  while !r && !li<>[] do
	    let (_,_,s') = X.predicate (List.hd !li) in
	    let q = Some(List.map2 (fun a a' -> 
	      Alg.toQuery (X.atoI (X.build (true,"=",[a;a'])))) s s') in
	    match (Alg.solve phi q) with
	    | None -> li := List.tl !li
	    | (Some l) -> r:=false; ans := l
	  done;
	  if !r then None else
	    let lis = (List.map X.itoA !ans) in
	    Some(List.fold_left (fun e a ->
	      ASet.add a e) ASet.empty ((List.hd !li)::lis))
	end

    let consistency atomN = 
      let l = ASet.fold (fun t l -> t::l) atomN [] in
      let l' = (List.fold_right (fun e li ->
	let (b,p,s) = X.predicate e in
	if p = "=" then e::li else li) l []) in
      if l' = l then 
	let phi = (List.map X.atoI l) in
	match (Alg.solve phi None) with
	| None -> None
	| (Some l) -> (*Alg.print_inputlist l; print_newline();*)
	  let li = (List.map X.itoA l) in
	  Some(List.fold_left (fun e a -> 
	    ASet.add a e) ASet.empty li)
      else begin
	let r = ref None in
	let phi = (List.map X.atoI l') in begin
	  match (Alg.solve phi None) with
	  | None -> ()
	  | (Some l) -> (*Alg.print_inputlist l; print_newline();*)
	    let li = (List.map X.itoA l) in
	    r := Some(List.fold_left (fun e a -> 
	      ASet.add a e) ASet.empty li)
	end;
	if !r <> None then !r else begin
	  let b = ref true and li = ref l and ans = ref ASet.empty in
	  while !b && !li<>[] do
	    let (_,p,_) = X.predicate (List.hd !li) in
	    if p <> "=" then
	      match (goal_consistency atomN (Atom.negation (List.hd !li))) with
	      | None -> li := List.tl !li
	      | Some s -> b:=false ; ans:=s
	  done;
	  if !b then None else
	    Some(ASet.add (List.hd !li) !ans)
	end 
      end

  end

end

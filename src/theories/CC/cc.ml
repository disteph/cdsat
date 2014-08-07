open Kernel
open Interfaces_I
open Theories
open AlgoCC
open PUFind

(* the theory CC(X) *)

module CCModulo (X : SolvableTheory) (U:PersistentUnionFind with type e = X.v 
and type d = X.input) : ThDecProc_tools.GThDecProc = struct
    
  module Sig = X.Sig

  module Atom = X.Atom

  let sugPlugin = None

  module Structure = X.Structure

  module Alg = Algo (X) (U)

  module Consistency(ASet : CollectImplem with type e = Atom.t) = struct
      
    let goal_consistency atomN t =
      if ASet.is_in t atomN then Some (ASet.add t ASet.empty)
      else
        begin
          Dump.msg (Some(fun p->p "Procedure called on goal %a under hypotheses\n%a" Atom.print_in_fmt t ASet.print_in_fmt atomN)) None None;
	  let l = ASet.fold (fun t l -> t::l) atomN [] in
	  let l' = (List.fold_left (fun li e ->
	    let (b,p,s) = X.predicate e in
	    if p = "=" then e::li else li) [] l) in
	  let phi = (List.map X.atoI l') in
	  let (b,p,s) = X.predicate t in
          let result = 
	    if p = "=" then
	      let q = Some([Alg.toQuery (X.atoI t)]) in
	      match (Alg.solve phi q) with
	      | None   -> None
	      | Some l -> (*Alg.print_inputlist l; print_newline();*)
	        let li = (List.map X.itoA l) in
	        Some(List.fold_left (fun e a -> ASet.add a e) ASet.empty li)
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
	        Some(List.fold_left (fun e a -> ASet.add a e) ASet.empty ((List.hd !li)::lis))
	    end
          in
          (match result with 
          | None   -> Dump.msg (Some(fun p->p "Procedure finished with CONSISTENT with hypotheses.")) None None
          | Some x -> Dump.msg (Some(fun p->p "Procedure finished with INCONSISTENT with hypotheses\n %a" ASet.print_in_fmt x)) None None); 
          result
        end

    let consistency atomN = 
      Dump.msg (Some(fun p->p "Procedure called on\n%a" ASet.print_in_fmt atomN)) None None;
      let l = ASet.fold (fun t l -> t::l) atomN [] in
      let l' = (List.fold_right (fun e li ->
	let (b,p,s) = X.predicate e in
	if p = "=" then e::li else li) l []) in
      let result = 
        if l' = l then 
          begin
	    let phi = List.map X.atoI l in
	    match Alg.solve phi None with
	    | None -> None
	    | Some l -> (*Alg.print_inputlist l; print_newline();*)
	      let li = List.map X.itoA l in
	      Some(List.fold_left (fun e a -> ASet.add a e) ASet.empty li)
          end
        else begin
	  let r = ref None in
	  let phi = List.map X.atoI l' in 
          begin
	    match Alg.solve phi None with
	    | None -> ()
	    | Some l -> (*Alg.print_inputlist l; print_newline();*)
	      let li = (List.map X.itoA l) in
	      r := Some(List.fold_left (fun e a -> ASet.add a e) ASet.empty li)
	  end;
 	  if !r <> None then !r else 
            begin
	      let b = ref true and li = ref l and ans = ref ASet.empty in
	      while !b && !li<>[] do
	        let (_,p,_) = X.predicate (List.hd !li) in
	        if p <> "=" then
                  begin
	            match goal_consistency atomN (Atom.negation (List.hd !li)) with
	            | None   -> ()
	            | Some s -> b:=false ; ans:=ASet.add (List.hd !li) s
                  end;
                li := List.tl !li;
	      done;
	      if !b then None else
	        Some(ASet.add (List.hd !li) !ans)
	    end 
        end
      in
      (match result with 
      | None   -> Dump.msg (Some(fun p->p "Procedure finished with CONSISTENT with hypotheses.")) None None
      | Some x -> Dump.msg (Some(fun p->p "Procedure finished with INCONSISTENT with hypotheses\n %a" ASet.print_in_fmt x)) None None); 
      result
  end

end

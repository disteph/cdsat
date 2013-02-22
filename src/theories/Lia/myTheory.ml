open Kernel

open Interfaces
open LibSimplex
open EqAst

module Atom =
  struct

  type t = EqAst.equation
  
  let equal = fun e1 e2 -> (e1 = e2)

  let compare = (Pervasives.compare : t -> t -> int)

  let negation = ForPsyche.negation

  let print_in_fmt = ForPsyche.print_in_fmt
  (* val print_in_fmt: Format.formatter -> t -> unit*)

  let toString = ForPsyche.toString
  (* val toString: t -> string *)

  let id = (Hashtbl.hash : t -> int)
  (* val id: t -> int *)

  let hash = (Hashtbl.hash : t -> int)
  (* val hash: t -> int *)

  let clear = fun _ -> ()
  (* val clear: unit -> unit *)
end

module DecProc (ASet : CollectImplem with type e = Atom.t)
 = struct
    let list_of_aset = fun aset ->
      ASet.fold (fun elt l -> (elt :: l)) aset []

    let aset_of_list = fun l ->
      List.fold_left (fun aset elt -> ASet.add elt aset) ASet.empty l

    let consistency = fun a ->
      let list_of_a = list_of_aset a in
      let lres = ForPsyche.test_inconsistency list_of_a in
      Core.fopt aset_of_list lres

    (*       val consistency: ASet.t -> ASet.t option *)

    let goal_consistency = fun a e ->
      let list_of_a = list_of_aset a in
      let lres = ForPsyche.test_goal_inconsistency list_of_a e in
      Core.fopt aset_of_list lres
  (*      val goal_consistency: ASet.t -> Atom.t -> ASet.t option*)
  end

module Parser (F:FormulaImplem with type lit = Atom.t)
  = struct

    let parse contents =
      let ast = Parsing_tools.ParsingPrimitives.smtlib2 contents in
	(* steph: now we have to traverse ast and build the corresponding formula;
	   not done yet; below is dummy *)
      let dummy_eq = 
        { eq_coeffs = Core.StringMap.empty ;
          eq_sign   = `Le ;
          eq_bound  = Core.Num.num_0;
	  id        = 0}
      in 
      let dummy_form = Lit dummy_eq
      in (F.build dummy_form : F.t)
    (* val parse   : string -> F.t *)

(* assia : for the time being the list of examples is empty *)      
      let examples = ([] : F.t list)
    (*  val examples: F.t list *)
  end
  

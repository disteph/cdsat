open Top
  
open Prop.Literals

open General
open Patricia
open SetConstructions

(* LSet = Sets of literals, patricia tries implementation (hconsed) *)

module I = TypesFromHConsed(LitF)
  
module DSet = struct
  type keys      = LitF.t
  let kcompare   = LitF.compare
  type infos     = unit
  let info_build = empty_info_build
  let treeHCons  = Some LitF.id
end

module LSet = PATSet.Make(DSet)(I)
  
(* Representation of terms for boolean reasoning, knowing about
   disjunction, negation, true and false *)
  
module ThDS = struct
  (* Representation of a clause, together with its negation: None is
     used to represent the trivially true clause, i.e. the negation of
     the empty clause. Apart from this case, a clause and its negation
     are usually Some a, Some b, with one of a or b being a singleton
     and the other one being non-empty. *)
    
  type t = (LSet.t option)*(LSet.t option)

  let build_lit b tag = Some(LSet.singleton(LitF.build(b,tag)))
  let bV tag _ = build_lit true tag, build_lit false tag
    
  (* Two clauses in a disjunction -> union, unless one is trivially
  true *)
  let or_comb = function
    | Some a, Some b -> Some(LSet.union a b)
    | _ -> None

  (* Two clauses in a conjunction -> abstract literal, unless one is
  trivially true *)
  let and_comb tag = function
    | None, a | a, None -> a
    | _ -> build_lit true tag

  let bC tag symb l = match symb,l with
    | Symbols.True, [] -> None, Some LSet.empty
    | Symbols.False,[] -> Some LSet.empty, None
    | Symbols.Or, [a,an; b,bn] -> or_comb(a,b), and_comb tag (an,bn)
    | Symbols.And,[a,an; b,bn] -> and_comb tag (a,b), or_comb(an,bn)
    | Symbols.Neg,[a,an] -> an,a
    | _,_ ->  bV tag l
end

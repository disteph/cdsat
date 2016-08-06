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
  type infos     = int
  let info_build = c_info_build
  let treeHCons  = Some LitF.id
end

module LSet = PATSet.Make(DSet)(I)
  
(* Representation of terms for boolean reasoning, knowing about
   disjunction, negation, true and false *)

type data = {
    aslit    : LitF.t;
    asclause : LSet.t option;
    nasclause: LSet.t option
  }
                         
module ThDS = struct
  (* Representation of a clause, together with its negation: None is
     used to represent the trivially true clause, i.e. the negation of
     the empty clause. Apart from this case, a clause and its negation
     are usually Some a, Some b, with one of a or b being a singleton
     and the other one being non-empty. *)
    
  type t = data

  let is_neg t =
    let b,_ = LitF.reveal t.aslit
    in not b
             
  let build_slit lit = Some(LSet.singleton lit)

  let build_lit lit = {
      aslit = lit;
      asclause = build_slit lit;
      nasclause = build_slit (LitF.negation lit)
    }

  (* Two clauses in a disjunction -> union, 
     unless one is trivially true *)
  let or_comb = function
    | Some a, Some b -> Some(LSet.union a b)
    | _ -> None

  (* Two clauses in a conjunction -> abstract literal, unless one is
  trivially true *)
  let and_comb lit = function
    | None, a | a, None -> a
    | _ -> build_slit lit

  let ttrue tag = {
      aslit = LitF.build(true,tag);
      asclause = None;
      nasclause = Some LSet.empty
    }

  let ffalse tag = {
      aslit = LitF.build(true,tag);
      asclause = Some LSet.empty;
      nasclause = None
    }

  let oor tag a b =
    let lit = LitF.build(true,tag) in
    {
      aslit = lit;
      asclause = or_comb(a.asclause,b.asclause);
      nasclause = and_comb (LitF.negation lit) (a.nasclause,b.nasclause)
    }

  let aand tag a b =
    let lit = LitF.build(true,tag) in
    {
      aslit = lit;
      asclause = and_comb lit (a.asclause,b.asclause);
      nasclause = or_comb (a.nasclause,b.nasclause)
    }

  let iimp tag a b =
    let lit = LitF.build(true,tag) in
    {
      aslit = lit;
      asclause = or_comb(a.nasclause,b.asclause);
      nasclause = and_comb (LitF.negation lit) (a.asclause,b.nasclause)
    }

  let negation t = {
      aslit = LitF.negation t.aslit;
      asclause = t.nasclause;
      nasclause = t.asclause;
    }
             

  let bV tag _ = build_lit (LitF.build(true,tag))
    

  let bC tag symb l = match symb,l with
    | Symbols.True, []  -> ttrue tag
    | Symbols.False,[]  -> ffalse tag
    | Symbols.Or, [a;b] -> oor tag a b
    | Symbols.And,[a;b] -> aand tag a b
    | Symbols.Imp,[a;b] -> iimp tag a b
    | Symbols.Neg,[a] when is_neg a -> build_lit(LitF.negation a.aslit)
    | Symbols.Neg,[a]   -> negation a
    | Symbols.IsTrue,[a] -> build_lit a.aslit
    | _,_ ->  bV tag l
end

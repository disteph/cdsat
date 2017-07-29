open Top
  
open Literals

open General
open Patricia
open Patricia_tools

(* LSet = Sets of literals, patricia tries implementation.
The only extra information that is stored is the cardinality of the set *)

module I = TypesFromHConsed(LitF)
  
module Arg = struct
  include LitF
  include CardInfo
  let treeHCons  = None (* Some LitF.id  *)
end

module LSet = struct
  include PatSet.Make(Arg)(I)
  let next lset = let lit = choose lset in lit, remove lit lset
end
  
(* Representation of terms for boolean reasoning *)                
                         
  (* asLit is the representation of the term as a literal
No other connective than negation is known in this representation,
but aslit for term t is equal to aslit for term not(not(t)).

asclause and nasclause are the representations of the term and its
negation, respectively, as clauses. In these representations, only
disjunctions, conjunctions, negation, and the constants true and false
are known.

None is used to represent the trivially true clause, i.e. the negation
of the empty clause. 
   *)

type t = {
    aslit    : LitF.t;
    asclause : LSet.t option;
    nasclause: LSet.t option
  }
           
module TS = struct
    
  type nonrec t = t
                    
  (* Building the unary clause containing literal lit *)
           
  let build_slit lit = Some(LSet.singleton lit)

  let build_lit lit = {
      aslit = lit;
      asclause = build_slit (LitF.negation lit);
      nasclause = build_slit lit
    }

  (* Two clauses in a disjunction -> union, 
     unless one is trivially true *)

  let or_comb = function
    | Some a, Some b -> Some(LSet.union a b)
    | _ -> None

  (* Two clauses in a conjunction:
     if one of them is trivially true -> other one,
     otherwise it is a abstract literal representing conjunction
     (has to be passed as argument *)

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
    
  let bB tag _ = build_lit (LitF.build(true,tag))

  let bC tag symb l = match symb,l with
    | Symbols.True, []  -> ttrue tag
    | Symbols.False,[]  -> ffalse tag
    | Symbols.Or, [a;b] -> oor tag a b
    | Symbols.And,[a;b] -> aand tag a b
    | Symbols.Imp,[a;b] -> iimp tag a b
    | Symbols.Neg,[a]   -> negation a
    | _,_ ->  bV tag l

end

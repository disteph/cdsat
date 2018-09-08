open Top
open Basic
open Terms
    
open General
open Patricia
open Patricia_tools

(* Type of maps from rational variables to rational coefficients *)
type varmap = (Term.t, bool, int, int, int*[`NoHCons]) Patricia.poly

(* Representation of terms for boolean reasoning *)                

(* asclause and ascube are the representations of the term and its
   negation, respectively, as clauses. For instance, (a \/ not b) is represented as
   { asclause = Some {a -> true, b -> false};
     ascube   = Some {(a \/ not b) -> false};
     freevar  = { a, b };
   }
   In these representations, only disjunctions, conjunctions, negation, and the
   constants true and false are known.
   None is used to represent the trivially true clause, i.e. the negation
   of the empty clause. 
*)

type t =
  { asclause : varmap option; (* None if trivially true *)
    ascube   : varmap option; (* None if trivially false *)
    freevar  : TSet.t }

module TS = struct
    
  type nonrec t = t

  let key = ThTermKey.make(module struct type nonrec t = t let name = "Clauses" end)

  (* VarMap = mapping variables to booleans, patricia tries implementation.
     The only extra information that is stored is the cardinality of the set *)

  module VarMap = struct
    include MapNH(struct
        include Term
        include CardInfo
        include TypesFromHConsed(Term)
        type values = bool
      end)
    let next varmap = let var,b = choose varmap in var,b, remove var varmap
    let pp_lit fmt (var,b) =
      Format.fprintf fmt "%s%a" (if b then "" else "~") Term.pp var
    let pp = print_in_fmt ~wrap:("{","}") pp_lit
  end

  let pp fmt t = match t.asclause with
    | Some asc -> VarMap.pp fmt asc
    | None -> Format.fprintf fmt "True_clause"
  let show t = Print.stringOf pp t

  (* Building the unary clause containing literal lit *)

  let build_slit var b = Some(VarMap.singleton var b)

  let build_lit var = { asclause = build_slit var true;
                        ascube   = build_slit var false;
                        freevar  = TSet.singleton var }

  exception Both

  let merge b1 b2 = if [%eq:bool] b1 b2 then b1 else raise Both

  (* Two clauses in a disjunction -> union, 
     unless one is trivially true;
     if one clause contains l and the other \neg l, then trivially true
  *)

  let or_comb = function
    | Some a, Some b ->
      (try Some(VarMap.union merge a b) with Both -> None)
    | _ -> None

  (* Two clauses in a conjunction:
     if one of them is trivially true -> other one,
     otherwise it is a abstract literal representing conjunction
     (has to be passed as argument *)

  let and_comb var b = function
    | None, a | a, None -> a
    | _ -> build_slit var b

  let ttrue = { asclause = None;
                ascube   = Some VarMap.empty;
                freevar  = TSet.empty }

  let ffalse = { asclause = Some VarMap.empty;
                 ascube   = None;
                 freevar  = TSet.empty }

  let oor var a b =
    { asclause = or_comb (a.asclause,b.asclause);
      ascube = and_comb var true (a.ascube,b.ascube);
      freevar = TSet.union a.freevar b.freevar }

  let aand var a b =
    { asclause = and_comb var false (a.asclause,b.asclause);
      ascube   = or_comb (a.ascube,b.ascube);
      freevar  = TSet.union a.freevar b.freevar }

  let iimp var a b =
    { asclause = or_comb (a.ascube,b.asclause);
      ascube   = and_comb var true (a.asclause,b.ascube);
      freevar  = TSet.union a.freevar b.freevar }

  let negation t = { asclause = t.ascube;
                     ascube   = t.asclause;
                     freevar  = t.freevar }

  let bC var symb l = match symb,l with
    | Symbols.True, []  -> ttrue
    | Symbols.False,[]  -> ffalse
    | Symbols.Or, [a;b] -> oor var a b
    | Symbols.And,[a;b] -> aand var a b
    | Symbols.Imp,[a;b] -> iimp var a b
    | Symbols.Neg,[a]   -> negation a
    | _,_ -> build_lit var

  let build t =
    match Term.reveal t with
    | C(symb,l) -> let l = List.map (proj key) l in bC t symb l
    | V _
    | FB(_,_,_) -> build_lit t

end

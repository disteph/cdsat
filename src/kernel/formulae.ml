(*****************************************************)
(* This file contains the implementation of formulae *)
(*****************************************************)

open Format
open Interfaces_theory

type ('a,'lit) form =
  | Lit of 'lit
  | TrueP
  | TrueN
  | FalseP
  | FalseN
  | AndP of 'a * 'a
  | OrP of 'a * 'a
  | AndN of 'a * 'a
  | OrN of 'a * 'a
  | ForAll of Sorts.t * 'a 
  | Exists of Sorts.t * 'a

module GForm = struct

  type ('a,'lit) t  = {reveal: ('a,'lit) revealt; id:int; data:'a}
  and  ('a,'lit) revealt = (('a,'lit) t,'lit) form

  let reveal f = f.reveal
  let id f     = f.id
  let data f   = f.data

  (* Displays a generic formula *)
  let print_in_fmt lit_print_in_fmt =
    let rec aux fmt f =
      match reveal f with
      | Lit l        -> lit_print_in_fmt fmt l
      | TrueP        -> fprintf fmt "%s" "\\trueP"
      | TrueN        -> fprintf fmt "%s" "\\trueN"
      | FalseP       -> fprintf fmt "%s" "\\falseP"
      | FalseN       -> fprintf fmt "%s" "\\falseN"
      | AndN(f1, f2) -> print_bin_op_in_fmt fmt f1 "\\andN" f2
      | OrN(f1, f2)  -> print_bin_op_in_fmt fmt f1 "\\orN" f2
      | AndP(f1, f2) -> print_bin_op_in_fmt fmt f1 "\\andP" f2
      | OrP(f1, f2)  -> print_bin_op_in_fmt fmt f1 "\\orP" f2
      | ForAll(so, f)-> print_quantif_in_fmt fmt "\\forall" so f
      | Exists(so, f)-> print_quantif_in_fmt fmt "\\exists" so f
    and print_bin_op_in_fmt fmt f1 op f2 =
      fprintf fmt "(%a %s %a)" aux f1 op aux f2
    and print_quantif_in_fmt fmt op so f =
      fprintf fmt "%s %a" op (* Sorts.print_in_fmt so *) aux f
    in aux

  (* Displays a generic formula paired with something *)
  let iprint_in_fmt aux1 aux2 fmt (f,tl) = fprintf fmt "%a%a" (print_in_fmt aux1) f aux2 tl

  let compare f1 f2 = Pervasives.compare f1.id f2.id

  (* Compares generic formulae paired with something *)
  let icompare aux (f1,tl1)(f2,tl2) =
    if compare f1 f2 = 0 then aux tl1 tl2
    else compare f1 f2

end

module type FormExtra = sig
  type t
  type lit
  val fdata_build: (t,lit) GForm.revealt -> t
end

module type FormulaType = sig
  type datatype
  type lit
  type t   = (datatype,lit) GForm.t
  val print_in_fmt : formatter -> t -> unit
  val iprint_in_fmt : (formatter -> 'subst -> unit) -> formatter -> (t*'subst) -> unit
  val compare : t -> t -> int
  val negation : t -> t
  val lit    : lit -> t
  val trueN  : t
  val trueP  : t
  val falseN : t
  val falseP : t
  val andN   : t * t -> t
  val andP   : t * t -> t
  val orN    : t * t -> t
  val orP    : t * t -> t
  val forall : Sorts.t * t -> t
  val exists : Sorts.t * t -> t
end

module Formula(Atom:AtomType)(Fdata: FormExtra with type lit = Atom.t)
  = struct

    type lit      = Atom.t
    type datatype = Fdata.t
    type t = (datatype,lit) GForm.t
      
    (* HashedType for formulae *)

    module MySmartFormulaImplemPrimitive = 
      (struct
        type t = (datatype,lit) GForm.revealt
        let equal t1 t2 =
	  match t1,t2 with
	  | Lit l1, Lit l2             -> l1==l2
	  | AndP (x1,x2), AndP (y1,y2) -> x1==y1 && x2==y2
	  | OrP (x1,x2), OrP (y1,y2)   -> x1==y1 && x2==y2
	  | AndN (x1,x2), AndN (y1,y2) -> x1==y1 && x2==y2
	  | OrN (x1,x2), OrN (y1,y2)   -> x1==y1 && x2==y2
	  | ForAll(so,x), ForAll(so',y)-> x==y && so=so'
	  | Exists(so,x), Exists(so',y)-> x==y && so=so'
	  | a, b                       -> a=b
        let hash t1 =
	  match t1 with
	  | Lit l        -> Atom.id l
	  | TrueP        -> 1
	  | TrueN        -> 2
	  | FalseP       -> 3
	  | FalseN       -> 4
	  | AndP (x1,x2) -> 5*x1.GForm.id+17*x2.GForm.id
	  | OrP (x1,x2)  -> 7*x1.GForm.id+19*x2.GForm.id
	  | AndN (x1,x2) -> 11*x1.GForm.id+23*x2.GForm.id
	  | OrN (x1,x2)  -> 13*x1.GForm.id+29*x2.GForm.id
          | ForAll(so,x) -> 31*x.GForm.id
          | Exists(so,x) -> 37*x.GForm.id
       end: Hashtbl.HashedType with type t = (datatype,lit) GForm.revealt)

    module H = Hashtbl.Make(MySmartFormulaImplemPrimitive)

    (* Constructing a formula with HConsing techniques *)

    let table = H.create 5003
    let funique =ref 0
    let build a =
      try H.find table a
      with Not_found -> 
        let f = {GForm.reveal =  a; GForm.id = !funique; GForm.data = Fdata.fdata_build a} in
        incr funique; H.add table a f; f

    let clear() = H.clear table

    let compare = GForm.compare

    (* Displays a formula *)
    let print_in_fmt = GForm.print_in_fmt Atom.print_in_fmt

    (* Displays a formula paired with something *)
    let iprint_in_fmt aux = GForm.iprint_in_fmt Atom.print_in_fmt aux

    (* Negates a formula *)
    let rec negation f =
      let f1 = match GForm.reveal f with
	| Lit t  -> Lit(Atom.negation t)
	| TrueP  -> FalseN
	| TrueN  -> FalseP
	| FalseP -> TrueN
	| FalseN -> TrueP
	| AndN(f1, f2) -> OrP(negation f1, negation f2)
	| OrN(f1, f2)  -> AndP(negation f1, negation f2)
	| AndP(f1, f2) -> OrN(negation f1, negation f2)
	| OrP(f1, f2)  -> AndN(negation f1, negation f2) 
	| ForAll(so,f) -> Exists(so,negation f) 
	| Exists(so,f) -> ForAll(so,negation f) 
      in
      build f1

    let lit a         = build(Lit a)
    let trueN         = build TrueN
    let trueP         = build TrueP
    let falseN        = build FalseN
    let falseP        = build FalseP
    let andN (f1, f2) = build(AndN(f1, f2))
    let andP (f1, f2) = build(AndP(f1, f2))
    let orN (f1, f2)  = build(OrN(f1, f2))
    let orP (f1, f2)  = build(OrP(f1, f2))
    let forall(so,f)  = build(ForAll(so,f))
    let exists(so,f)  = build(Exists(so,f))
  end

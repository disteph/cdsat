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

module M = struct

  type ('a,'lit) t = ('a,'lit) form

  let equal eqRec eqLit t1 t2 =
    match t1,t2 with
    | Lit l1, Lit l2             -> eqLit l1 l2
    | AndP (x1,x2), AndP (y1,y2) -> eqRec x1 y1 && eqRec x2 y2
    | OrP (x1,x2), OrP (y1,y2)   -> eqRec x1 y1 && eqRec x2 y2
    | AndN (x1,x2), AndN (y1,y2) -> eqRec x1 y1 && eqRec x2 y2
    | OrN (x1,x2), OrN (y1,y2)   -> eqRec x1 y1 && eqRec x2 y2
    | ForAll(so,x), ForAll(so',y)-> eqRec x y && so=so'
    | Exists(so,x), Exists(so',y)-> eqRec x y && so=so'
    | a, b                       -> false

  let hash hRec hLit = function
    | Lit l        -> hLit l
    | TrueP        -> 1
    | TrueN        -> 2
    | FalseP       -> 3
    | FalseN       -> 4
    | AndP (x1,x2) -> 5*(hRec x1)+17*(hRec x2)
    | OrP (x1,x2)  -> 7*(hRec x1)+19*(hRec x2)
    | AndN (x1,x2) -> 11*(hRec x1)+23*(hRec x2)
    | OrN (x1,x2)  -> 13*(hRec x1)+29*(hRec x2)
    | ForAll(so,x) -> 31*(hRec x)
    | Exists(so,x) -> 37*(hRec x)

end

module Formula = struct

  include HCons.MakePoly(M)

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

  (* Compares generic formulae paired with something *)
  let icompare aux (f1,tl1)(f2,tl2) =
    if compare f1 f2 = 0 then aux tl1 tl2
    else compare f1 f2

  module type Extra = sig
    type t
    type lit
    val build: (lit,t) revealed -> t
  end

  module Make(Atom:AtomType)(Fdata: Extra with type lit = Atom.t)
    = struct

      include InitData(Basic.HashedTypeFromHCons(Atom))(struct type t = Fdata.t let build _ = Fdata.build end)

      type lit      = Atom.t
      type datatype = Fdata.t

      let compare = compare

    (* Displays a formula *)
      let print_in_fmt = print_in_fmt Atom.print_in_fmt

    (* Displays a formula paired with something *)
      let iprint_in_fmt aux = iprint_in_fmt Atom.print_in_fmt aux

    (* Negates a formula *)
      let rec negation f =
        let f1 = match reveal f with
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

  module type S = sig
    type datatype
    type lit
    type t   = (lit,datatype) generic
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

end

open Printf
open Format
open Interfaces

(* Generic code providing standard functions about formulae *)

module PrintableFormula (Atom: AtomType)(F: FormulaImplem with type lit = Atom.t) 
  = struct

    include F
	
    (* Displays a formula *)
    let rec print_in_fmt fmt f =
      match F.reveal f with
	| Lit l -> Atom.print_in_fmt fmt l
	| TrueP -> fprintf fmt "%s" "\\trueP"
	| TrueN -> fprintf fmt "%s" "\\trueN"
	| FalseP -> fprintf fmt "%s" "\\falseP"
	| FalseN -> fprintf fmt "%s" "\\falseN"
	| AndN(f1, f2) -> print_bin_op_in_fmt fmt f1 "\\andN" f2
	| OrN(f1, f2) -> print_bin_op_in_fmt fmt f1 "\\orN" f2
	| AndP(f1, f2) -> print_bin_op_in_fmt fmt f1 "\\andP" f2
	| OrP(f1, f2) -> print_bin_op_in_fmt fmt f1 "\\orP" f2
    and print_bin_op_in_fmt fmt f1 op f2 =
      fprintf fmt "(%a %s %a)" print_in_fmt f1 op print_in_fmt f2

    let toString f =
      let buf = Buffer.create 255 in
	fprintf (formatter_of_buffer buf) "%a%!" print_in_fmt f;
	Buffer.contents buf

    (* Negates a formula *)
    let rec negation f =
      let f1 = match F.reveal f with
	| Lit t  -> Lit(Atom.negation t)
	| TrueP  -> FalseN
	| TrueN  -> FalseP
	| FalseP -> TrueN
	| FalseN -> TrueP
	| AndN(f1, f2) -> OrP(negation f1, negation f2)
	| OrN(f1, f2) -> AndP(negation f1, negation f2)
	| AndP(f1, f2) -> OrN(negation f1, negation f2)
	| OrP(f1, f2) -> AndN(negation f1, negation f2) in
	F.build f1

    let lit a         = F.build(Lit a)
    let trueN         = F.build TrueN
    let trueP         = F.build TrueP
    let falseN        = F.build FalseN
    let falseP        = F.build FalseP
    let andN (f1, f2) = F.build(AndN(f1, f2))
    let andP (f1, f2) = F.build(AndP(f1, f2))
    let orN (f1, f2)  = F.build(OrN(f1, f2))
    let orP (f1, f2)  = F.build(OrP(f1, f2))
  end

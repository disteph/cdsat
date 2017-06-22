open Format
open General

open Top
open Symbols
open Interfaces_basic
open Basic
open Tools
open Variables
open Specs

open Termstructures.Literals

type 'a free = private Free
type bound   = private Bound

type (_,_) form =
| LitF : LitF.t -> (_,_ free) form
| LitB : LitB.t -> (_,bound) form
| TrueP: (_,_) form
| TrueN: (_,_) form
| FalseP: (_,_) form
| FalseN: (_,_) form
| AndP  : 'a * 'a -> ('a,_) form
| OrP   : 'a * 'a -> ('a,_) form
| AndN  : 'a * 'a -> ('a,_) form
| OrN   : 'a * 'a -> ('a,_) form
| ForAllF: Sorts.t * 'a * FVSubst.t -> (_,'a free) form
| ExistsF: Sorts.t * 'a * FVSubst.t -> (_,'a free) form
| ForAllB: Sorts.t * 'a -> ('a,bound) form
| ExistsB: Sorts.t * 'a -> ('a,bound) form


type (_,_) func_prim =
| BoundFunc : (bound,_) func_prim
| FreeFunc : ('a -> 'b) -> ('a free,'b free) func_prim

type ('a,'b) func = ('a,'b free) func_prim

let equal (type a)(type b) (eqSub:(b,(b,bool)func)func) eqRec (t1:(a,b)form) (t2:(a,b)form) =
  match t1,t2 with
  | LitF l1, LitF l2           -> LitF.equal l1 l2
  | LitB l1, LitB l2           -> LitB.equal l1 l2
  | AndP (x1,x2), AndP (y1,y2) 
  | OrP (x1,x2), OrP (y1,y2)   
  | AndN (x1,x2), AndN (y1,y2) 
  | OrN (x1,x2), OrN (y1,y2)   -> eqRec x1 y1 && eqRec x2 y2
  | ForAllF(so,x,d), ForAllF(so',y,d')
    -> let FreeFunc eqSub1 = eqSub in
       let FreeFunc eqSub2 = eqSub1 x in
       eqSub2 y && FVSubst.equal d d' && Sorts.equal so so'
  | ExistsF(so,x,d), ExistsF(so',y,d')
    -> let FreeFunc eqSub1 = eqSub in
       let FreeFunc eqSub2 = eqSub1 x in
       eqSub2 y && FVSubst.equal d d' && Sorts.equal so so'
  | ForAllB(so,x), ForAllB(so',y)-> eqRec x y && Sorts.equal so so'
  | ExistsB(so,x), ExistsB(so',y)-> eqRec x y && Sorts.equal so so'
  | TrueP, TrueP | TrueN, TrueN
  | FalseP,FalseP | FalseN, FalseN -> true
  | _, _                       -> false


let hash (type a)(type b) (hSub:(b,int)func) hRec : (a,b)form -> int  = function
  | LitF l       -> LitF.hash l
  | LitB l       -> LitB.hash l
  | TrueP        -> 1
  | TrueN        -> 2
  | FalseP       -> 3
  | FalseN       -> 4
  | AndP (x1,x2) -> 5*(hRec x1)+17*(hRec x2)
  | OrP (x1,x2)  -> 7*(hRec x1)+19*(hRec x2)
  | AndN (x1,x2) -> 11*(hRec x1)+23*(hRec x2)
  | OrN (x1,x2)  -> 13*(hRec x1)+29*(hRec x2)
  | ForAllF(so,x,d) -> let FreeFunc hSub1 = hSub in
                       31*(hSub1 x)*(FVSubst.hash d)
  | ExistsF(so,x,d) -> let FreeFunc hSub1 = hSub in
                       37*(hSub1 x)*(FVSubst.hash d)
  | ForAllB(so,x) -> 31*(hRec x)
  | ExistsB(so,x) -> 37*(hRec x)


(* Displays a formula *)
let print_in_fmt_latex (type a)(type b) ?print_atom (pSub:(b,formatter->unit)func) pRec =
  let print_bin_op_in_fmt fmt f1 op f2 =
    fprintf fmt "(%a %s %a)" pRec f1 op pRec f2
  and print_quantif_in_fmt fmt op so pSub1 f d =
    fprintf fmt "%s %a %a" op (* Sorts.pp so *) pSub1 f FVSubst.pp d
  and print_quantif_in_fmtB fmt op so f =
    fprintf fmt "%s %a" op (* Sorts.pp so *) pRec f
  in let aux fmt: (a,b)form -> unit = function
  | LitF l       -> fprintf fmt "%a" (LitF.print_in_fmt ?print_atom) l
  | LitB l       -> fprintf fmt "%a" LitB.pp l
  | TrueP        -> fprintf fmt "%s" "\\trueP"
  | TrueN        -> fprintf fmt "%s" "\\trueN"
  | FalseP       -> fprintf fmt "%s" "\\falseP"
  | FalseN       -> fprintf fmt "%s" "\\falseN"
  | AndN(f1, f2) -> print_bin_op_in_fmt fmt f1 "\\andN" f2
  | OrN(f1, f2)  -> print_bin_op_in_fmt fmt f1 "\\vee" f2
  (* | OrN(f1, f2)  -> print_bin_op_in_fmt fmt f1 "\\orN" f2 *)
  | AndP(f1, f2) -> print_bin_op_in_fmt fmt f1 "\\wedge" f2
  (* | AndP(f1, f2) -> print_bin_op_in_fmt fmt f1 "\\andP" f2 *)
  | OrP(f1, f2)  -> print_bin_op_in_fmt fmt f1 "\\orP" f2
  | ForAllF(so,f,d)-> let FreeFunc pif = pSub in
                      print_quantif_in_fmt fmt "\\forall" so (fun fmt t -> pif t fmt) f d
  | ExistsF(so,f,d)-> let FreeFunc pif = pSub in
                      print_quantif_in_fmt fmt "\\exists" so (fun fmt t -> pif t fmt) f d
  | ForAllB(so, f)-> print_quantif_in_fmtB fmt "\\forall" so f
  | ExistsB(so, f)-> print_quantif_in_fmtB fmt "\\exists" so f
     in fun reveal fmt t -> aux fmt (reveal t)

(* Displays a formula *)
let print_in_fmt_utf8 (type a)(type b) ?print_atom (pSub:(b,formatter->unit)func) pRec =
  let print_bin_op_in_fmt fmt f1 op f2 =
    fprintf fmt "(%a %s %a)" pRec f1 op pRec f2
  and print_quantif_in_fmt fmt op so pSub1 f d =
    fprintf fmt "%s %a %a" op (* Sorts.pp so *) pSub1 f FVSubst.pp d
  and print_quantif_in_fmtB fmt op so f =
    fprintf fmt "%s %a" op (* Sorts.pp so *) pRec f
  in let aux fmt: (a,b)form -> unit = function
  | LitF l       -> fprintf fmt "%a" (LitF.print_in_fmt ?print_atom) l
  | LitB l       -> fprintf fmt "%a" LitB.pp l
  | TrueP        -> fprintf fmt "%s" "⊤+"
  | TrueN        -> fprintf fmt "%s" "⊤-"
  | FalseP       -> fprintf fmt "%s" "⊥+"
  | FalseN       -> fprintf fmt "%s" "⊥-"
  | AndN(f1, f2) -> print_bin_op_in_fmt fmt f1 "∧-" f2
  | OrN(f1, f2)  -> print_bin_op_in_fmt fmt f1 "∨-" f2
  | AndP(f1, f2) -> print_bin_op_in_fmt fmt f1 "∧+" f2
  | OrP(f1, f2)  -> print_bin_op_in_fmt fmt f1 "∨+" f2
  | ForAllF(so,f,d)-> let FreeFunc pif = pSub in
                      print_quantif_in_fmt fmt "∀" so (fun fmt t -> pif t fmt) f d
  | ExistsF(so,f,d)-> let FreeFunc pif = pSub in
                      print_quantif_in_fmt fmt "∃" so (fun fmt t -> pif t fmt) f d
  | ForAllB(so, f)-> print_quantif_in_fmtB fmt "∀" so f
  | ExistsB(so, f)-> print_quantif_in_fmtB fmt "∃" so f
     in fun reveal fmt t -> aux fmt (reveal t)

let print_in_fmt ?print_atom fmt = match !Dump.display with
  | Dump.Latex -> print_in_fmt_latex ?print_atom fmt
  | _ -> print_in_fmt_utf8 ?print_atom fmt
                           
(* Negates a formula *)
let negation (type a)(type b) (nSub:(b,b)func_prim) nRec reveal build =
  let aux : (a,b)form -> (a,b)form = function
    | LitF l  -> LitF(LitF.negation l)
    | LitB l  -> LitB(LitB.negation l)
    | TrueP   -> FalseN
    | TrueN   -> FalseP
    | FalseP  -> TrueN
    | FalseN  -> TrueP
    | AndN(f1, f2)   -> OrP(nRec f1, nRec f2)
    | OrN(f1, f2)    -> AndP(nRec f1, nRec f2)
    | AndP(f1, f2)   -> OrN(nRec f1, nRec f2)
    | OrP(f1, f2)    -> AndN(nRec f1, nRec f2) 
    | ForAllF(so,f,d)-> let FreeFunc negB = nSub in
                        ExistsF(so,negB f,d) 
    | ExistsF(so,f,d)-> let FreeFunc negB = nSub in
                        ForAllF(so,negB f,d) 
    | ForAllB(so,f)  -> ExistsB(so,nRec f)
    | ExistsB(so,f)  -> ForAllB(so,nRec f)
  in fun t -> build(aux(reveal t))


module Abbrev
         (I: sig
              type a
              type b
              val build:(a,b)form->a
            end)
  = struct
  let trueN         = I.build TrueN
  let trueP         = I.build TrueP
  let falseN        = I.build FalseN
  let falseP        = I.build FalseP
  let andN (f1, f2) = I.build(AndN(f1, f2))
  let andP (f1, f2) = I.build(AndP(f1, f2))
  let orN (f1, f2)  = I.build(OrN(f1, f2))
  let orP (f1, f2)  = I.build(OrP(f1, f2))
end


module F = struct
  type 'a t = ('a, Terms.TermB.t free) form
  let equal eqRec = equal (FreeFunc(fun x->FreeFunc(Terms.TermB.equal x))) eqRec 
  let hash hRec = hash (FreeFunc Terms.TermB.hash) hRec
end

module FormulaF = struct

  include HCons.Make(F)

  let print_in_fmt ?print_atom fmt t =
    let rec aux fmt t = print_in_fmt ?print_atom (FreeFunc(fun t fmt->Terms.TermB.pp fmt t)) aux reveal fmt t
    in aux fmt t

  module type Extra = sig
    type t
    val build: t g_revealed -> t
  end

  module type S = sig
    type datatype

    type t = datatype generic [@@deriving eq,hash,ord,show]
    val id : t -> int
    val clear : unit -> unit
    val print_in_fmt : ?print_atom:(formatter -> int -> unit) -> formatter -> t -> unit

    type revealed  = datatype g_revealed

    val negation : t -> t

    val lit    : LitF.t -> t
    val trueN  : t
    val trueP  : t
    val falseN : t
    val falseP : t
    val andN   : t * t -> t
    val andP   : t * t -> t
    val orN    : t * t -> t
    val orP    : t * t -> t
    val forall : Sorts.t * Terms.TermB.t * FVSubst.t -> t
    val exists : Sorts.t * Terms.TermB.t * FVSubst.t -> t
      
    val bV : int -> FreeVar.t -> t
    val bC : int -> Symbols.t  -> t list -> t
    val bB : int -> (Sorts.t*Terms.TermB.t*FVSubst.t) -> t
  end

  module Make(Fdata: Extra): (S with type datatype = Fdata.t) = struct

    include InitData(HCons.NoBackIndex)(struct
                type t = Fdata.t
                let build _ = Fdata.build
              end)

    type datatype = Fdata.t

    let compare = compare
    let id = id
    let print_in_fmt = print_in_fmt
    let pp fmt = print_in_fmt fmt
    let show = Dump.stringOf pp


    let negation =
      let negB tB = match Terms.TermB.reveal tB with
        | Terms.C(Symbols.Neg,[tB']) -> tB'
        | _ -> Terms.TermB.bC Symbols.Neg [tB]
      in
      let rec aux t = negation (FreeFunc negB) aux reveal build t
      in aux

    include Abbrev(struct
                type a = t
                type b = Terms.TermB.t free
                let build = build
              end)
    let lit l        = build(LitF l)
    let forall(so,f,d)  = build(ForAllF(so,f,d))
    let exists(so,f,d)  = build(ExistsF(so,f,d))

    let bV tag _ = lit(LitF.build(true,tag))

    let bC tag =
      let const c = function
        | [] -> c
        | _  -> raise(ModelError "ModelError_Formulae.ml: Expected 0 arguments")
      and mono f = function
        | [a] -> f a
        | _ -> raise(ModelError "ModelError_Formulae.ml: Expected 1 arguments")
      and bi f = function
        | [a;b] -> f(a,b)
        | _ -> raise(ModelError "ModelError_Formulae.ml: Expected 2 arguments")
      in function
      | True       -> const trueP
      | False      -> const falseN
      | Neg        -> mono negation
      | And        -> bi andP
      | Or         -> bi orN
      | Imp        -> bi (fun(a,b) -> orN(negation a,b))
      | Xor        -> bi (fun(a,b) -> andP(orN(a,b),orN(negation a,negation b)))
      | Eq Sorts.Prop  -> bi (fun(a,b) -> andP(orN(negation a,b),orN(negation b,a)))
      | NEq Sorts.Prop -> bi (fun(a,b) -> orN(andP(a,negation b),andP(b,negation a)))
      | _          -> bV tag

    let bB = bV
           
  end

end
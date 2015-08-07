open Format

open Top
open Symbol
open Interfaces_basic
open Basic
open Literals
open Specs

type 'a free = private Free
type bound = private Bound

type (_,_) form =
| Lit  : LitF.t -> (_,_ free) form
| LitB : LitB.t -> (_,bound) form
| TrueP: (_,_) form
| TrueN: (_,_) form
| FalseP: (_,_) form
| FalseN: (_,_) form
| AndP  : 'a * 'a -> ('a,_) form
| OrP   : 'a * 'a -> ('a,_) form
| AndN  : 'a * 'a -> ('a,_) form
| OrN   : 'a * 'a -> ('a,_) form
| ForAll: Sorts.t * 'a * DSubst.t -> (_,'a free) form
| Exists: Sorts.t * 'a * DSubst.t -> (_,'a free) form
| ForAllB: Sorts.t * 'a -> ('a,bound) form
| ExistsB: Sorts.t * 'a -> ('a,bound) form


type (_,_) func_prim =
| BoundFunc : (bound,_) func_prim
| FreeFunc : ('a -> 'b) -> ('a free,'b free) func_prim

type ('a,'b) func = ('a,'b free) func_prim


module LitFHashed = HashedTypeFromHCons(LitF)
module LitBHashed = HashedTypeFromHCons(LitB)
module DSubstHashed  = HashedTypeFromHCons(DSubst)

let equal (type a)(type b) (eqSub:(b,(b,bool)func)func) eqRec (t1:(a,b)form) (t2:(a,b)form) =
  match t1,t2 with
  | Lit l1, Lit l2             -> LitFHashed.equal l1 l2
  | LitB l1, LitB l2           -> LitBHashed.equal l1 l2
  | AndP (x1,x2), AndP (y1,y2) 
  | OrP (x1,x2), OrP (y1,y2)   
  | AndN (x1,x2), AndN (y1,y2) 
  | OrN (x1,x2), OrN (y1,y2)   -> eqRec x1 y1 && eqRec x2 y2
  | ForAll(so,x,d), ForAll(so',y,d')-> let FreeFunc eqSub1 = eqSub in
                                       let FreeFunc eqSub2 = eqSub1 x in
                                       eqSub2 y && DSubstHashed.equal d d' && so=so'
  | Exists(so,x,d), Exists(so',y,d')-> let FreeFunc eqSub1 = eqSub in
                                       let FreeFunc eqSub2 = eqSub1 x in
                                       eqSub2 y && DSubstHashed.equal d d' && so=so'
  | ForAllB(so,x), ForAllB(so',y)-> eqRec x y && so=so'
  | ExistsB(so,x), ExistsB(so',y)-> eqRec x y && so=so'
  | TrueP, TrueP | TrueN, TrueN
  | FalseP,FalseP | FalseN, FalseN -> true
  | _, _                       -> false


let hash (type a)(type b) (hSub:(b,int)func) hRec : (a,b)form -> int  = function
  | Lit l        -> LitFHashed.hash l
  | LitB l       -> LitBHashed.hash l
  | TrueP        -> 1
  | TrueN        -> 2
  | FalseP       -> 3
  | FalseN       -> 4
  | AndP (x1,x2) -> 5*(hRec x1)+17*(hRec x2)
  | OrP (x1,x2)  -> 7*(hRec x1)+19*(hRec x2)
  | AndN (x1,x2) -> 11*(hRec x1)+23*(hRec x2)
  | OrN (x1,x2)  -> 13*(hRec x1)+29*(hRec x2)
  | ForAll(so,x,d) -> let FreeFunc hSub1 = hSub in
                      31*(hSub1 x)*(DSubstHashed.hash d)
  | Exists(so,x,d) -> let FreeFunc hSub1 = hSub in
                      37*(hSub1 x)*(DSubstHashed.hash d)
  | ForAllB(so,x) -> 31*(hRec x)
  | ExistsB(so,x) -> 37*(hRec x)


(* Displays a formula *)
let print_in_fmt (type a)(type b) (pSub:(b,formatter->unit)func) pRec =
  let print_bin_op_in_fmt fmt f1 op f2 =
    fprintf fmt "(%a %s %a)" pRec f1 op pRec f2
  and print_quantif_in_fmt fmt op so pSub1 f d =
    fprintf fmt "%s %a %a" op (* Sorts.print_in_fmt so *) pSub1 f DSubst.print_in_fmt d
  and print_quantif_in_fmtB fmt op so f =
    fprintf fmt "%s %a" op (* Sorts.print_in_fmt so *) pRec f
  in let aux fmt: (a,b)form -> unit = function
  | Lit  l       -> fprintf fmt "%a" LitF.print_in_fmt l
  | LitB l       -> fprintf fmt "%a" LitB.print_in_fmt l
  | TrueP        -> fprintf fmt "%s" "\\trueP"
  | TrueN        -> fprintf fmt "%s" "\\trueN"
  | FalseP       -> fprintf fmt "%s" "\\falseP"
  | FalseN       -> fprintf fmt "%s" "\\falseN"
  | AndN(f1, f2) -> print_bin_op_in_fmt fmt f1 "\\andN" f2
  | OrN(f1, f2)  -> print_bin_op_in_fmt fmt f1 "\\orN" f2
  | AndP(f1, f2) -> print_bin_op_in_fmt fmt f1 "\\andP" f2
  | OrP(f1, f2)  -> print_bin_op_in_fmt fmt f1 "\\orP" f2
  | ForAll(so,f,d)-> let FreeFunc pif = pSub in
                     print_quantif_in_fmt fmt "\\forall" so (fun fmt t -> pif t fmt) f d
  | Exists(so,f,d)-> let FreeFunc pif = pSub in
                     print_quantif_in_fmt fmt "\\exists" so (fun fmt t -> pif t fmt) f d
  | ForAllB(so, f)-> print_quantif_in_fmtB fmt "\\forall" so f
  | ExistsB(so, f)-> print_quantif_in_fmtB fmt "\\exists" so f
     in fun reveal fmt t -> aux fmt (reveal t)


(* Negates a formula *)
let negation (type a)(type b) (nSub:(b,b)func_prim) nRec reveal build =
  let aux : (a,b)form -> (a,b)form = function
    | Lit  l  -> Lit(LitF.negation l)
    | LitB l  -> LitB(LitB.negation l)
    | TrueP   -> FalseN
    | TrueN   -> FalseP
    | FalseP  -> TrueN
    | FalseN  -> TrueP
    | AndN(f1, f2)   -> OrP(nRec f1, nRec f2)
    | OrN(f1, f2)    -> AndP(nRec f1, nRec f2)
    | AndP(f1, f2)   -> OrN(nRec f1, nRec f2)
    | OrP(f1, f2)    -> AndN(nRec f1, nRec f2) 
    | ForAll(so,f,d) -> let FreeFunc negB = nSub in
                        Exists(so,negB f,d) 
    | Exists(so,f,d) -> let FreeFunc negB = nSub in
                        ForAll(so,negB f,d) 
    | ForAllB(so,f)  -> ExistsB(so,nRec f)
    | ExistsB(so,f)  -> ForAllB(so,nRec f)
  in fun t -> build(aux(reveal t))


module Abbrev
  (I: sig
    type a
    type b
    val build:(a,b)form->a
  end)
  =
struct
  let trueN         = I.build TrueN
  let trueP         = I.build TrueP
  let falseN        = I.build FalseN
  let falseP        = I.build FalseP
  let andN (f1, f2) = I.build(AndN(f1, f2))
  let andP (f1, f2) = I.build(AndP(f1, f2))
  let orN (f1, f2)  = I.build(OrN(f1, f2))
  let orP (f1, f2)  = I.build(OrP(f1, f2))
end




module B = struct
  type 'a t = ('a,bound) form
  let equal eqRec = equal BoundFunc eqRec 
  let hash hRec = hash BoundFunc hRec 
end

module FormulaB = struct

  include HCons.Make(B)
  include Init(HCons.NoBackIndex)

  let print_in_fmt =
    let rec aux fmt t = print_in_fmt BoundFunc aux reveal fmt t
    in aux

  let negation =
    let rec aux t = negation BoundFunc aux reveal build t
    in aux

  include Abbrev(struct
    type a = t
    type b = bound
    let build = build
  end)

  let lit (b,term) = build(LitB(LitB.build(b,term)))
  let forall(so,a) = build(ForAllB(so,a))
  let exists(so,a) = build(ExistsB(so,a))

end



module FormulaBHashed = HashedTypeFromHCons(FormulaB)

module F = struct
  type 'a t = ('a, FormulaB.t free) form
  let equal eqRec = equal (FreeFunc(fun x->FreeFunc(FormulaBHashed.equal x))) eqRec 
  let hash hRec = hash (FreeFunc FormulaBHashed.hash) hRec
end

module FormulaF = struct

  include HCons.Make(F)

  let print_in_fmt =
    let rec aux fmt t = print_in_fmt (FreeFunc(fun t fmt->FormulaB.print_in_fmt fmt t)) aux reveal fmt t
    in aux

  module type Extra = sig
    type t
    val build: t g_revealed -> t
  end

  module type S = sig
    type datatype

    include PHCons with type t = datatype generic

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
    val forall : Sorts.t * FormulaB.t * DSubst.t -> t
    val exists : Sorts.t * FormulaB.t * DSubst.t -> t

    val semantic : Symbol.t  -> (t list -> t) option
    val leaf     : IntSort.t -> t
  end

  module Make(Fdata: Extra)
    = (struct

      include InitData(HCons.NoBackIndex)(struct type t = Fdata.t let build _ = Fdata.build end)

      type datatype = Fdata.t

      let compare = compare
      let id = id
      let print_in_fmt = print_in_fmt

      let negation =
        let rec aux t = negation (FreeFunc FormulaB.negation) aux reveal build t
        in aux

      include Abbrev(struct
        type a = t
        type b = FormulaB.t free
        let build = build
      end)
      let lit l        = build(Lit l)
      let forall(so,f,d)  = build(ForAll(so,f,d))
      let exists(so,f,d)  = build(Exists(so,f,d))

      let leaf is = lit(LitF.build(true,is))

      let semantic =
        let const c = Some(function
          | [] -> c
          | _  -> raise(ModelError "ModelError_Formulae.ml: Expected 0 arguments"))
        and mono f = Some(function
          | [a] -> f a
          | _ -> raise(ModelError "ModelError_Formulae.ml: Expected 1 arguments"))
        and bi f = Some(function
          | [a;b] -> f(a,b)
          | _ -> raise(ModelError "ModelError_Formulae.ml: Expected 2 arguments"))
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
        | _             -> None

    end:S with type datatype = Fdata.t)

end

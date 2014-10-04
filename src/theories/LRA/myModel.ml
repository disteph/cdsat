open Kernel.Interfaces_I

(* open Lib.Sums *)
open LibSimplex.Core
open Num
open MyAtom

open SMTLib2

let ctimes s(p,r)=
  (StringMap.map (fun _ c -> s */ c) p, s */ r)

let times (p,r)(p',r')=
  if StringMap.is_empty p then ctimes r(p',r')
  else if StringMap.is_empty p' then ctimes r'(p,r)
  else failwith("Not a linear multiplication!!!")

let cdiv (p,r) s =
  (StringMap.map (fun _ c -> c // s) p, r // s)

let div (p,r)(p',r')=
  if StringMap.is_empty p' then cdiv (p,r)r'
  else failwith("Not a linear division!!!")

let plus (p,r)(p',r')=
  (StringMap.union (fun c c' -> c +/ c') p p',r +/ r')

let minus a b=
  plus a (ctimes (Int(-1)) b)

let simplify_novar n = function
  | `Gt -> Num.compare_num n num_0 > 0 
  | `Ge -> Num.compare_num n num_0 >= 0
  | `Lt -> Num.compare_num n num_0 < 0
  | `Le -> Num.compare_num n num_0 <= 0

module Structure(F:Kernel.Formulae.FormulaType with type lit = Atom.t) = struct

  open Theories
  module PS = Tools.Prop.Structure(F)

  type t = 
    | Rat of (StringMap.t*Num.num) PS.ite
    | Prop of F.t

  let build_lit_base sign (p,r) =
    if StringMap.is_empty p
    then if simplify_novar r sign then F.trueP else F.falseN
    else F.lit(Atom.build sign p (num_0 -/ r))

  let build_lit_aux sign c =
    match sign with
      | `EqRat -> F.andP(build_lit_base `Le c,build_lit_base `Ge c)
      | `NEqRat-> F.orP (build_lit_base `Lt c,build_lit_base `Gt c)
      | `Ge | `Gt | `Le | `Lt as g -> build_lit_base g c

  let build_lit sign c =
    PS.bool_simpl(PS.map (build_lit_aux sign) c)

  let toform = function
    | Prop f -> f
    | _      -> raise (ModelError "ModelError: trying to convert into a formula an expression that clearly is not one")

  let st = 
    { sigsymb_i =
        (fun (symb:ThSig_register.LRASig.symbol) l ->
	  match symb, l with
	  | `CstRat i,[]         -> Rat(PS.Leaf(StringMap.empty,i))
	  | `Plus,[Rat a;Rat b]  -> Rat(PS.mmap plus a b)
	  | `Minus,[Rat a;Rat b] -> Rat(PS.mmap minus a b)
	  | `Times,[Rat a;Rat b] -> Rat(PS.mmap times a b)
	  | `Divide,[Rat a;Rat b]-> Rat(PS.mmap div a b)
	  | `Op,[Rat a]          -> Rat(PS.map (ctimes (Int(-1))) a)
	  | `ITERat,[Prop c;Rat a;Rat b] -> Rat(PS.INode(c,a,b))
	  | `Ge, [Rat a;Rat b]
	  | `Gt, [Rat a;Rat b]
	  | `Le, [Rat a;Rat b]
	  | `Lt, [Rat a;Rat b]
	  | `EqRat,[Rat a;Rat b]
	  | `NEqRat,[Rat a;Rat b] as g
	    -> Prop(build_lit (fst g) (PS.mmap minus a b))
	  | s,l                  -> Prop(PS.symb_i s (List.map toform l)));

      decsymb_i =
	(let rec aux so var l = 
	  if l <> [] then
	    raise (ModelError ("ModelError: variable "^var^" shouldn't have any argument"))
	  else match so with
	  | `Rat  -> Rat(PS.Leaf(StringMap.add (StringComparable.build var) (fun _ -> Int 1) StringMap.empty,num_0))
	  | `Prop ->
	    match aux `Rat var [] with
	    | Rat a -> Prop(build_lit `Gt a)
	    | _     -> raise (ModelError
				("ModelError: should not happen (whilst converting a boolean variable "^var^" to an atom "^var^" >0 )"))
	in aux);

        boundsymb_i = (fun db so -> raise (ModelError ("ModelError: cannot treat bound variables")));
        quantif_i = (fun db so sf -> raise (ModelError ("ModelError: cannot treat quantifiers")))

    }

  let examples = []

end

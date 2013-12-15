(********************)
(* Model primitives *)
(********************)

open Theories
open ThSig_register

module PropStructure(F:Kernel.Interfaces.PrintableFormulaType) = struct

  type 'a ite = 
    | Leaf of 'a
    | INode of F.t*('a ite)*('a ite)

  let rec map_aux f = function
    | Leaf a'      -> f a'
    | INode(c,d,e) -> INode(c,map_aux f d,map_aux f e)

  let map f    = map_aux (fun x -> Leaf(f x))

  let mmap f a = map_aux (fun y -> map (fun x->f x y) a)

  let rec bool_simpl = function
    | Leaf a      -> a
    | INode(c,d,e)-> F.orN(F.andP(c,bool_simpl d),F.andP(F.negation c,bool_simpl e))

  let symb_i (symb: [> PropSig.symbol]) (formulalist:F.t list) = 
    match symb, formulalist with
      | `True,[]      -> F.trueN
      | `False,[]     -> F.falseN
      | `Neg,[a]      -> F.negation a
      | `And,[a;b]    -> F.andP(a,b)
      | `Or,[a;b]     -> F.orN(a,b)
      | `Imp,[a;b]    -> F.orN(F.negation a,b)
      | `Xor,[a;b]    -> F.andP(F.orN(a,b),F.orN(F.negation a,F.negation b))
      | `EqProp,[a;b] -> F.andP(F.orN(F.negation a,b),F.orN(F.negation b,a))
      | `NEqProp,[a;b] -> F.orN(F.andP(a,F.negation b),F.andP(b,F.negation a))
      | `ITEProp,[a;b;c] -> bool_simpl(INode(a,Leaf b,Leaf c))
      | _             -> raise (ModelError "ModelError: Not the right number of arguments")

end

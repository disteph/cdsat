(****************************)
(* Tools for Prop signature *)
(****************************)

open Top
open Symbol

open Formulae

module Intern(F: Formula.S) = struct

  (* let rec bool_simpl = function *)
  (*   | Leaf a      -> a *)
  (*   | INode(c,d,e)-> F.orN(F.andP(c,bool_simpl d),F.andP(F.negation c,bool_simpl e)) *)


  (* val sigsymb_i   : Sig.symbol -> t list -> t *)

  let semantic symb l =
      match symb, l with
      | True,[]       -> F.trueN
      | False,[]      -> F.falseN
      | Neg,[a]       -> F.negation (a)
      | And,[a;b]     -> F.andP(a,b)
      | Or, [a;b]     -> F.orN(a,b)
      | Imp,[a;b]     -> F.orN(F.negation a,b)
      | Xor,[a;b]     -> F.andP(F.orN(a,b),F.orN(F.negation a,F.negation b))
      | Eq Sorts.Prop, [a;b] -> F.andP(F.orN(F.negation a,b),F.orN(F.negation b,a))
      | NEq Sorts.Prop,[a;b] -> F.orN(F.andP(a,F.negation b),F.andP(b,F.negation a))
      | Forall so,[a]  -> F.forall(so,a)
      | Exists so,[a]  -> F.exists(so,a)
      (* | ITEProp,[a;b;c] -> bool_simpl(INode(a,Leaf b,Leaf c)) *)
      | _             -> failwith ""

end

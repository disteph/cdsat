open Flags
open LibSimplex
open EqAst

module Atom =
  struct

  type t = EqAst.equation

  let reveal at = (at.eq_coeffs,at.eq_sign,at.eq_bound)

  let hash e    = Hashtbl.hash(reveal e)
  (* val hash: t -> int *)

  module H = Hashtbl.Make(struct
    type t1 = t
    type t = t1
    let hash = hash
    let equal e1 e2 = 
      let (m1,s1,c1) = reveal e1 in
      let (m2,s2,c2) = reveal e2 in
	(Core.StringMap.compare m1 m2 ==0)&&(s1=s2)&&(Num.compare_num c1 c2 ==0)
  end)

  let table = H.create 5003

  let atomid = ref 0

  let build sign a b =
    let f = { eq_coeffs = a ;
	      eq_sign   = sign ;
	      eq_bound  = b;
	      id        = !atomid} in
    try H.find table f
    with Not_found ->
      (* print_endline(string_of_int(!atomid));*)
	    incr atomid;
      H.add table f f;
      f
  
  let id e = e.id
  (* val id: t -> int *)

  let compare e1 e2 = Pervasives.compare (e1.id)(e2.id)
  let equal e1 e2 = (compare e1 e2 = 0)

  let negation e =
    let newsign = match e.eq_sign with
      |`Le -> `Gt
      |`Ge -> `Lt
      |`Lt -> `Ge
      |`Gt -> `Le
    in build newsign e.eq_coeffs e.eq_bound

  let print_in_fmt = ForPsyche.print_in_fmt
  (* val print_in_fmt: Format.formatter -> t -> unit*)

  let toString = ForPsyche.toString
  (* val toString: t -> string *)

  let clear () = 
    Core.StringComparable.clear();
    atomid := 0;
    H.clear table
  (* val clear: unit -> unit *)
end

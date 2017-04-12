(* ******************************************* *)
(* Implementation of sets of atoms with Lists,
   Implementation of formulae info with Lists,
   Implementation of sets of formulae with Lists *)
(* ******************************************* *)

open Format

open Kernel

open Top.Interfaces_basic

open Prop
open Literals
open Formulae

module type PrintableType = sig 
  type t [@@deriving eq]
  val print_in_fmt: ?print_atom:(formatter -> int -> unit) -> formatter -> t -> unit
end

module MyCollectImplem (MyPType:PrintableType) = struct
  type e = MyPType.t
  type t = e list

  let mem = List.mem MyPType.equal

  let empty = []

  let singleton e = [e]

  let add x l = if mem x l then l else x::l

  let rec remove x = function
    | [] -> failwith(Dump.toString (fun p->
                         p "%a is not in list!" (fun fmt -> MyPType.print_in_fmt fmt) x))
    | y::l when MyPType.equal y x -> l
    | y::l -> y::(remove x l)

  let rec union gamma1 = function
    | [] -> gamma1
    | a::gamma2 when mem a gamma1 -> union gamma1 gamma2
    | a::gamma2 -> a::(union gamma1 gamma2)

  let rec inter gamma1 = function
    | [] -> []
    | a::gamma2 -> let gamma3 = inter gamma1 gamma2 in
	           if mem a gamma1 then a::gamma3 else gamma3

  let rec diff gamma1 gamma = match gamma1 with
    | [] -> []
    | a::gamma2 -> let gamma3 = diff gamma2 gamma in
	           if mem a gamma then gamma3 else a::gamma3

  let rec fold f l0 init= match l0 with
    | a::l -> fold f l (f a init)
    | [] -> init

  let rec print_in_fmt ?print_atom fmt = function
    | []    -> ()
    | f::[] -> fprintf fmt "%a" (MyPType.print_in_fmt ?print_atom)f
    | f::l  -> fprintf fmt "%a, %a" (MyPType.print_in_fmt ?print_atom) f (print_in_fmt ?print_atom) l

  let choose = function 
    | [] -> None
    | a::_ -> Some a

end

module Generate = struct

  module UASet = MyCollectImplem(LitF)

  module UF = struct
    type t   = unit
    let build f = ()
  end

  module UFSet = MyCollectImplem(struct
    type t = UF.t FormulaF.generic
    let equal l1 l2 = FormulaF.compare l1 l2 =0
    let print_in_fmt = FormulaF.print_in_fmt
  end)

end

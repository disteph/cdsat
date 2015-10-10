(* ******************************************* *)
(* Implementation of sets of atoms with Lists,
   Implementation of formulae info with Lists,
   Implementation of sets of formulae with Lists *)
(* ******************************************* *)

open Format

open General
open Kernel

open Top.Interfaces_basic
open Prop
open Literals
open Formulae
open Interfaces_theory

module type PrintableType = sig 
  type t 
  val print_in_fmt: formatter -> t -> unit
end

module MyCollectImplem (MyPType:PrintableType) = struct
  type e = MyPType.t
  type t = e list
  let is_empty = function 
    | [] -> true
    | _ -> false
  let mem = List.mem
  let empty = []
  let add x l = if mem x l then l else x::l
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
  let rec remove x = function
    | [] -> failwith(Dump.toString (fun p->p "%a is not in list!" MyPType.print_in_fmt x))
    | y::l when y=x -> l
    | y::l -> y::(remove x l)
  let next = function
    | (a::l) -> (a,l)
    | [] -> failwith("No more item to pick")
  let rec fold f l0 init= match l0 with
    | (a::l) -> fold f l (f a init)
    | [] -> init
  let subset gamma1 gamma2 =
    fold (fun a b ->b && mem a gamma2) gamma1 true

  let rec print_in_fmt fmt = function
    | []    -> ()
    | f::[] -> fprintf fmt "%a" MyPType.print_in_fmt f
    | f::l  -> fprintf fmt "%a, %a" MyPType.print_in_fmt f print_in_fmt l

end

module Generate = struct

  module UASet = MyCollectImplem(LitF)

  module UF = struct
    type t   = unit
    let build f = ()
  end

  module UFSet = MyCollectImplem(struct
    type t = UF.t FormulaF.generic
    let print_in_fmt = FormulaF.print_in_fmt
  end)

end

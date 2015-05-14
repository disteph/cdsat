(* ******************************************* *)
(* Implementation of sets of atoms with Lists,
   Implementation of formulae info with Lists,
   Implementation of sets of formulae with Lists *)
(* ******************************************* *)

open Format

open General
open Kernel

open Interfaces_basic
open Interfaces_theory
open Formulae

module type PrintableType = sig 
  type t 
  val print_in_fmt: formatter -> t -> unit
end

module MyCollectImplem (MyPType:PrintableType)
  : (CollectTrusted with type e = MyPType.t
                    and  type t = MyPType.t list) =
struct
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

module Generate(DS:TheoryDSType) = struct

  open DS

  type dsubsts = DSubst.t

  module UASet = MyCollectImplem(IAtom)

  module UF = struct
    type lit = Atom.t
    type t   = unit
    let fdata_build f = ()
  end

  module UFSet = MyCollectImplem(struct
    type t = (UF.t,UF.lit) GForm.t * dsubsts
    let print_in_fmt = GForm.iprint_in_fmt Atom.print_in_fmt DSubst.print_in_fmt
  end)

end

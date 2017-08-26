open Format
open General
open Top
open Interfaces_basic
open Basic
open Specs
open Variables

module LitF = struct

  include HCons.Make(struct
    type 'a t = bool*int [@@deriving eq,hash]
    let name = "LitF"
  end)

  include Init(HCons.NoBackIndex)

  let print_in_fmt ?print_atom fmt l =
    let b,a = reveal l in
    let print_atom fmt =
      match print_atom, !Dump.display with
      | Some f,_ -> f fmt a
      | None,Dump.Latex -> fprintf fmt "l_{%i}" a
      | None,_          -> fprintf fmt "l_%i" a
    in
    match !Dump.display with
    | Dump.Latex -> fprintf fmt "%s{%t}" (if b then "" else "\\overline") print_atom
    | _ -> fprintf fmt "%s%t" (if b then "" else "¬") print_atom

  let pp fmt t = print_in_fmt fmt t
  let show = Print.stringOf pp
  let negation l = 
    let b,a = reveal l in build(not b,a)

end


module LitB = struct

  module LF = struct
    type 'a t = bool*Terms.TermB.t [@@deriving eq,hash,show]
    let name = "LitB"
  end

  include HCons.Make(LF)
  include Init(HCons.NoBackIndex)

  let pp fmt l =
    let b,a = reveal l in
    match !Dump.display with
    | Dump.Latex -> fprintf fmt "%s{%a}" (if b then "" else "\\overline") Terms.TermB.pp a
    | _ -> fprintf fmt "%s%a" (if b then "" else "¬") Terms.TermB.pp a

  let show = Print.stringOf pp

  let negation l = 
    let b,a = reveal l in build(not b,a)

end

module TS = struct
  type t = LitF.t 
  let bV tag _ = LitF.build(true,tag)
  let bB tag _ = LitF.build(true,tag)
  let bC tag symb l = match symb,l with
    | Symbols.Neg,[a] -> LitF.negation a
    | _,_ ->  bV tag l
end

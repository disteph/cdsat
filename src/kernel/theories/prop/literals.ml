open Format

open Top
open Interfaces_basic
open Basic
open Specs
open Variables

module LitF = struct

  include HCons.Make(struct
    type _ t = bool*int
    let equal _ (b,a) (b',a') = b=b' && a=a'
    let hash _ (b,a) = (if b then 2 else -3) * a
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
                
  let negation l = 
    let b,a = reveal l in build(not b,a)

end


module BoundVar = struct
  include IntSort
  let get_sort db = let (_,so) = reveal db in so
  let get_from_context db context = let (i,_) = reveal db in context i
end

module TermB = Terms.Make(BoundVar)(Terms.EmptyData(BoundVar))

type termB = (BoundVar.t,unit) Terms.term

module LitB = struct

  module LF = struct
    type _ t = bool*termB
    let equal _ (b,a) (b',a') = b=b' && Terms.equal a a'
    let hash _ (b,a) = (if b then 2 else -3) * Terms.hash a
  end

  include HCons.Make(LF)
  include Init(HCons.NoBackIndex)

  let print_in_fmt fmt l =
    let b,a = reveal l in
    match !Dump.display with
    | Dump.Latex -> fprintf fmt "%s{%a}" (if b then "" else "\\overline") TermB.print_in_fmt a
    | _ -> fprintf fmt "%s%a" (if b then "" else "¬") TermB.print_in_fmt a

  let negation l = 
    let b,a = reveal l in build(not b,a)

end

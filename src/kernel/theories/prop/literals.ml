open Format

open Top
open Interfaces_basic
open Basic
open Specs

module IntSortHashed = HashedTypeFromHCons(IntSort)

module LitF = struct

  include HCons.Make(struct
    type _ t = bool*IntSort.t
    let equal _ (b,a) (b',a') = b=b' && IntSortHashed.equal a a'
    let hash _ (b,a) = (if b then 2 else -3) * IntSortHashed.hash a
  end)

  include Init(HCons.NoBackIndex)

  let print_in_fmt fmt l =
    let b,a = reveal l in
    fprintf fmt "%s{%a}" (if b then "" else "\\overline") IntSort.print_in_fmt a

  let negation l = 
    let b,a = reveal l in build(not b,a)

end

module TermB = Terms.Make(IntSort)(Terms.EmptyData(IntSort))

module LitB = struct

  module LF = struct
    type _ t = bool*(unit term)
    let equal _ (b,a) (b',a') = b=b' && Terms.equal a a'
    let hash _ (b,a) = (if b then 2 else -3) * Terms.hash a
  end

  include HCons.Make(LF)
  include Init(HCons.NoBackIndex)

  let print_in_fmt fmt l =
    let b,a = reveal l in
    fprintf fmt "%s{%a}" (if b then "" else "\\overline") TermB.print_in_fmt a

  let negation l = 
    let b,a = reveal l in build(not b,a)

end

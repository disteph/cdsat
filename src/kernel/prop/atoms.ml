open Format

open Top
open Interfaces_basic

type ('a,'b) prod = private Prod

module M = struct

  type (_,_) t =
    AtomCons : bool * Symbol.t * (('leaf,'datatype) Terms.term list)
    -> ('t,('leaf,'datatype)prod) t

  let equal (type a)(type b) _ _ 
      (AtomCons(b, a, tl)   :(a,b)t)
      (AtomCons(b', a', tl'):(a,b)t) = 
    b = b' && a = a' && Terms.equaltl (tl, tl')

  let hash (type a)(type b) _ _ (AtomCons(b, a, tl):(a,b)t) =
    (if b then 0 else 1) + 2 * Hashtbl.hash a + 3 * Terms.hashtl tl

end

module H = HCons.MakePoly(M)

type ('leaf,'datatype) atom = (('leaf,'datatype)prod, unit) H.generic

module type S = sig
  type leaf
  module Term : Terms.S with type leaf := leaf
  type t = (leaf,Term.datatype) atom
  val reveal  : t -> bool * Symbol.t * (Term.t list)
  val id      : t -> int 
  val compare : t -> t -> int
  val build    : bool * Symbol.t * Term.t list -> t
  val clear    : unit -> unit
  val print_in_fmt: Format.formatter -> t -> unit
  val negation : t -> t
  module Homo(Mon: MonadType) : sig
    val lift : 
      ('a -> leaf Mon.t) -> ('a,Term.datatype) atom -> t Mon.t
  end
end

module Make
  (Leaf : PHCons)
  (Data : Terms.DataType with type leaf := Leaf.t) =
struct

  type leaf = Leaf.t
  type t = (leaf,Data.t) atom

  module Term = Terms.Make(Leaf)(Data)
  module I = H.Init(struct
    type t = (Leaf.t,Data.t) prod
    let equal _ _ = failwith "equal function on type (Leaf.t,Data.t) prod should not exist"
    let hash _ = failwith "hash function on type (Leaf.t,Data.t) prod should not exist"
  end)

  let reveal a = match H.reveal a with
    | M.AtomCons(b,p,tl) -> (b,p,tl)
  let id = H.id
  let compare = H.compare

  let build(b,p,tl) = I.build(M.AtomCons(b,p,tl))
  let clear () = Term.clear(); I.clear()

  let print_in_fmt fmt t =
    match reveal t with
    | true, s, tl ->
      if tl<>[] 
      then fprintf fmt "{%a%a}" Symbol.print_in_fmt s Term.printtl_in_fmt tl
      else fprintf fmt "{%a}" Symbol.print_in_fmt s
    | false, s, tl ->
      if tl<>[] 
      then fprintf fmt "\\overline {%a}%a" Symbol.print_in_fmt s Term.printtl_in_fmt tl
      else fprintf fmt "\\overline {%a}" Symbol.print_in_fmt s

  let negation t = let (b, a, tl) = reveal t in build(not b, a, tl)

  module Homo(Mon: MonadType) = struct

    let lift update at = 
      let module THomo = Term.Homo(Mon) in
      let (b,p,tl) = reveal at in
      Mon.bind (fun l -> Mon.return(build(b,p,l))) (THomo.lifttl update tl)

  end
end

module Lit = Make(Basic.IntSort)(Terms.EmptyData(Basic.IntSort))

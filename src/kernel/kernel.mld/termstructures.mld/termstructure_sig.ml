open Top.Terms

module type Type = sig
  type t
  val key : t ThTermKey.t
  val build : (Term.t -> t) -> Term.t -> t
end


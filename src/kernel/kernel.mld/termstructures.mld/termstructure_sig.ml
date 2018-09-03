open Top.Terms

module type Type = sig
  type t
  val key : t ThTermKey.t
  (* Achtung: in (build t),
     build should NOT call Terms.data on term t but only on its subterms (if at all),
     otherwise this gives an infinite loop *)
  val build : Term.t -> t
end


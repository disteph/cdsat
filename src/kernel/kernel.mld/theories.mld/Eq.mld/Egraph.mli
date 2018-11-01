open General.Sums

open Top
open Terms
open Values

include module type of Egraph_sig
       
(* Sum type for terms+values *)
module TermValue : sig
  type t = (Term.t,Value.t values) sum [@@deriving eq,ord,show,hash]          
end

module Make(W: Writable) : S with type termvalue := TermValue.t

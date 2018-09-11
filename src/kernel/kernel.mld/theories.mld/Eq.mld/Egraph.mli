open General.Sums

open Top
open Terms
open Messages
open Values

open Interfaces
       
module Make(W: Writable) : sig
  
  (* Abbreviation for single assignments *)
  type stop = (unit,straight) message list * (unit,unsat) message
  
  (* Sum type for terms+values *)
  module TermValue : sig
    type t = (Term.t,Value.t values) sum [@@deriving eq,ord,show,hash]          
  end

  module EG : Egraph with type stop := stop
end



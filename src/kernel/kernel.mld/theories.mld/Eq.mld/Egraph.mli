open General.Sums

open Top
open Specs
open Sassigns
open Messages

open Interfaces
       
module Make(DS: GlobalDS) : sig
  
  open DS

  (* Abbreviation for single assignments *)
  type stop = (unit,straight) Msg.t list * (unit,unsat) Msg.t
  
  (* Sum type for terms+values *)
  module TermValue : sig
    type t = (Term.t,Value.t values) sum
               [@@deriving eq,ord,show,hash]                               
  end

  module EG : Egraph with type cval  := CValue.t
                      and type stop  := stop
                      and type term  := Term.t
                      and type value := Value.t
end



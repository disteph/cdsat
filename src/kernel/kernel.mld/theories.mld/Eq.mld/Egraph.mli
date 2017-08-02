open General.Sums

open Top
open Specs
open Sassigns
open Messages

open Interfaces
       
module Make(DS: GlobalDS) : sig
  
  open DS

  (* Abbreviation for single assignments *)
  type straight = (unit,Assign.t*bassign,Messages.straight) message
  type stop = straight list * ((unit,Assign.t*bassign,unsat) message)

  (* The information we want to keep about each component *)
  type info              

  (* Sum type for terms+values *)
  module TermValue : sig
    type t = (Term.t,Value.t values) sum
               [@@deriving eq,ord,show,hash]                               
  end

  module Make(REG : RawEgraph with type node = TermValue.t
                               and type edge = (bassign,sassign)sum
                               and type info = info)
         : (Egraph with type info := info
                                 and type cval := CValue.t
                                              and type stop := stop
                                                           and type term := Term.t
                                                                        and type value := Value.t)
end



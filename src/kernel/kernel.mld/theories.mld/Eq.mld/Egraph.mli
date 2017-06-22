open General.Sums

open Top
open Specs
open Messages

open Interfaces
       
module Make(DS: DSproj) : sig
  
  open DS

  (* Abbreviation for single assignments *)
  type sassign = Term.t*(Value.t Values.t) [@@deriving eq,show]
  type boolassign = Term.t*bool [@@deriving eq,show]
  type straight = (unit,Assign.t*boolassign,Messages.straight) message
  type stop = straight list * ((unit,Assign.t*boolassign,unsat) message)

  (* The information we want to keep about each component *)
  type info              

  (* Sum type for terms+values *)
  module TermValue : sig
    type t = (Term.t,Value.t Values.t) sum
               [@@deriving eq,ord,show,hash]                               
  end

  module Make(REG : RawEgraph with type node := TermValue.t
                               and type edge := sassign
                               and type info := info)
         : (Egraph with type info := info
                                 and type cval := CValue.t
                                              and type stop := stop
                                                           and type term := Term.t
                                                                        and type sassign := sassign
                                                                                        and type termValue := TermValue.t)
end



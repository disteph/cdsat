(****************)
(* Open Modules *)
(****************)

open Format

open Interfaces_basic
open Basic

(* Useful abbreviations for module types *)

module type BuilderType = sig
  type t
  val semantic : Symbol.t  -> (t list -> t) option
  val leaf     : IntSort.t -> t
end


module type TermType = TermDef.S with type leaf := IntSort.t

(* Useful abbreviation for term type *)

type 'a term = (IntSort.t,'a) TermDef.term

(* The type of answers that a theory should produce, when queried *)

module OM = struct

  type ('a,'b) answer = private 
                        | UNSAT of 'b
                        | SAT   of ('a,'b) resume
                        | GimmeFreshVar of Sorts.t*(World.FreeVar.t -> ('a,'b) resume)
                        | Write of 'b * (('a,'b) resume)
                            
  and ('a,'b) resume = 'b -> (('a,'b) answer)

(* (\* The module type of a theory that knows of the type gen of *)
(*    aggregated datastructures used by the theories *\) *)

(*   module type TheoryType = sig *)
(*     type gen *)
(*     type tset *)
(*     val init     : (gen,tset) resume *)
(*     val examples : ((unit-> gen term)*bool) list *)
(*   end *)

(* The module type of a theory that does not yet know of the type gen
   of aggregated datastructures used by the theories: it should first
   provide the datatype for its own representation of terms (module
   Builder), then it should be able to receive the module of terms with
   the aggregated representation and a projection function from this
   aggregated datatype to its own term representation type, and produce
   one of the above *)

  module type Type = sig

    val names    : string list
    val sugPlugin: string option

    module Builder : BuilderType

    val make
      : ('a -> Builder.t)
      -> (module TermType with type datatype = 'a)
      -> (module CollectTrusted with type e = 'a term and type t = 'b)
      -> ('a,'b) resume

  end

end

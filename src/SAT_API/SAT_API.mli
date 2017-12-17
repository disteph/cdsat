open General.Sums
open PluginsTh.PluginTh

module type Syntax = sig
  
  type a = string

  module Formula : sig
    type t = private
           | Top
           | Btm
           | Atom of a
           | And of t * t
           | Or of t * t
           | Implies of t * t
                     
    val atom_t : a -> t
    val and_t : t -> t -> t
    val or_t : t -> t -> t
    val implies_t : t -> t -> t
    val top_t : unit -> t
    val btm_t : unit -> t
    val atom_equal : t -> t -> bool
  end

  module Sequent : sig
    type t
    val build_sequent : Formula.t list -> Formula.t -> t
    val left : t -> Formula.t list
    val right : t -> Formula.t
  end

end
                   
module Solve(S:Syntax) : sig

  open S
  type sequent = Formula.t list * Formula.t list
  type proof =
    | Trivial : sequent -> proof
    | Unsat   : sequent * proof -> proof
    | Resolve : sequent * proof * proof -> proof
    | Curry   : sequent * proof -> proof

  type answer =
    | SAT of (S.a * bool) list
    | UNSAT of proof

  val prove : S.Sequent.t -> answer

end

  


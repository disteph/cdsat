open General.Sums
       
open Top
open Terms
open Sassigns
open Messages

open Theories
open Theory
open Register
       
module type Proof = sig

  type 'a t

  val from_th : _ propa t
  val unsat   : unsat t
  val sat     : sat t

  (* Resolves H⊢L and H',L⊢A into H∪H'⊢A *)
  val resolve  : straight t -> 'a propa t -> 'a propa t

  (* Turns H,H',L⊢⊥ into (essentially) H⊢H'⇒~L
     First argument is H' (empty if not given), second is L (⊤↦true if not given) *)
  val curryfy  : ?assign:Assign.t -> ?flip:BAssign.t -> unsat t -> straight t

end

(* Data-structures used as a result of theory interactions *)
module type WhiteBoard = sig

  module W : Writable

  type 'a proof

  (* Type of theory messages after they have been "signed"
     with the appropiate theory handlers. 
     It's basically a message, together with the set of handlers
     of the theories that signed/contributed to the message *)
  type 'a t = private WB of unit HandlersMap.t * (unit,'a) message * 'a proof
  type any = Any : _ t -> any [@@unboxed] [@@deriving show]

  val pp : 'a t Format.printer

  (* Signing messages: one function for standard theories, one for the equality theory *)
  val sign    : ('a*_) Tags.t -> ('a, 'b) message -> 'b t
  val sign_Eq : (Eq.MyTheory.sign, 'b) message -> 'b t

  (* Turns a propagation message H⊢L into H,~L⊢⊥ *)
  val unsat    : straight t -> unsat t

  (* Resolves H⊢L and H',L⊢A into H∪H'⊢A *)
  val resolve  : straight t -> 'b propa t -> 'b propa t

  (* Turns H,H',L⊢⊥ into (essentially) H⊢H'⇒~L
     First argument is H' (empty if not given), second is L (⊤↦true if not given) *)
  val curryfy  : ?assign:Assign.t -> ?flip:BAssign.t -> unsat t -> straight t

  (* Type where we collect data about theories
     that have said they were happy with the current model *)
  type sat_tmp = private {
    assign  : Assign.t; (* The current model *)
    sharing : TSet.t;   (* The shared terms *)
    (* Handlers of theories that have not said they were happy *)
    left    : unit HandlersMap.t;
    (* List of sets of Σ-variables of the problem -one set per Σ/theory -
       to make sure we have a model, pairwise intersections of these sets
       should be included in sharing *)
    varlist : TSet.t Lazy.t list }

  (* When a new theory says it is happy with the model,
     we update the data with function sat below; it outputs
     - GoOn(updated data)  if all went well but some theories still need to speak up;
     - Share(set of terms) if we have discovered new shared terms:
                           all theories need to take them into account;
     - Done(assign,shared) if all theories have agreed on model assign sharing shared
     - NoModelMatch assign
     if theory says it is happy with another model than it should (i.e. than assign)
     - NoSharingMatch sharing if it is happy with the expected model
       but thinks the shared terms are not what they should be (i.e. sharing) *)

  type sat_ans = private
    | GoOn of sat_tmp
    | Share of TSet.t
    | Done of Assign.t * TSet.t
    | NoModelMatch of Assign.t
    | NoSharingMatch of TSet.t

  (* When no theory has yet said that it was happy: left and varlist are empty *)
  val sat_init : Assign.t -> sharing:TSet.t -> sat_tmp

  val sat : sat t -> sat_tmp -> sat_ans
end

(* Kernel's API *)
module type API = sig

  (* WhiteBoard module as defined above *)
  module WB : WhiteBoard
    
  (* E-graph module *)
  module EGraph : Eq.MyTheory.API with type sign := Eq.MyTheory.sign
  (* List of theory modules - kernel part *)
  val th_modules : Modules.t list

  (* The parsing function - necessarily part of the trusted base *)
  val parse : Parsers.Register.t -> string -> Term.t list
end

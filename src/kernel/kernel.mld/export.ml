open General
open Sums
open Patricia
open Patricia_tools
       
open Top
open Interfaces_basic
open Messages
open Theories
open Register
open Specs
open Sassigns
       
(* Datatypes constructed after we know which theories participate to the combination *)
module type GlobalImplem = sig
  (* Datatype for hash-consed single assignments (full module SAssign below) *)
  type sassign_hconsed
  (* GlobalDS are the global datastructures as specified in top/specs.ml *)
  include GlobalDS with type Assign.t = (sassign_hconsed,unit,int,int,EmptyInfo.infos) poly
  module SAssign : sig
    include PHCons with type t = sassign_hconsed
    val reveal : t -> sassign
    val build  : sassign -> t
  end
end

(* Data-structures used as a result of theory interactions *)
module type WhiteBoard = sig

  module DS : GlobalImplem
  open DS
  
  (* Type of theory messages after they have been "signed"
     with the appropiate theory handlers. 
     It's basically a message, together with the set of handlers
     of the theories that signed/contributed to the message *)
  type 'a t = private WB of unit HandlersMap.t * (unit,'a) Msg.t * 'a proof

  and 'a proof = private
    | Blob  : 'a proof
    | UnsatProof   : straight t -> unsat proof
    | ResolveProof : straight t * 'b propa t -> 'b propa proof
    | CurryProof   : bassign * unsat t -> straight proof
  
  val pp : Format.formatter -> 'a t -> unit

  (* Signing messages: one function for standard theories, one for the equality theory *)
  val sign    : (_*('a*_*_*_)) Tags.t -> ('a, 'b) Msg.t -> 'b t
  val sign_Eq : (Eq.MyTheory.sign, 'b) Msg.t -> 'b t

  (* Turns a propagation message H⊢L into H,~L⊢⊥ *)
  val unsat    : straight t -> unsat t
  (* Resolves H⊢L and H',L⊢A into H∪H'⊢A *)
  val resolve  : straight t -> 'b propa t -> 'b propa t
  (* Turns H,H',L⊢⊥ into (essentially) H⊢H'⇒~L
     First argument is H' (empty if not given), second is L (⊤↦true if not given) *)
  val curryfy  : ?assign:Assign.t -> ?flip:bassign -> unsat t -> straight t

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

  (* When no theory has yet said that it was happy: left and varlist are empty *)
  val sat_init : Assign.t -> sharing:TSet.t -> sat_tmp

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
                                        
  val sat : sat t -> sat_tmp -> sat_ans
end

type (_,_) proj =
  | Proj : ('cv -> 'v values option) -> ('cv,'v has_values) proj
  | NoProj : ('cv,has_no_values) proj

(* Kernel's API *)
module type API = sig
  (* WhiteBoard module as defined above *)
  module WB : WhiteBoard
  open WB.DS

  (* E-graph module *)
  module EGraph : Eq.Interfaces.API with type sign := Eq.MyTheory.sign
                                     and type termdata := Term.datatype
                                     and type value  := Value.t
                                     and type cval   := CValue.t
                                     and type assign := Assign.t
                                     and type tset   := TSet.t
  (* List of theory modules - kernel part *)
  val th_modules : (Term.datatype*Value.t*Assign.t*TSet.t) Modules.t list

  (* Function to extract from a global value a theory-specific value *)
  val vproj      : (_ * (_ * _ * 'd * _)) Theories.Register.Tags.t
                   -> (CValue.t, 'd) proj

  (* The parsing function - necessarily part of the trusted base *)
  val parse : Parsers.Register.t -> string -> Term.t list
end

(* Extended API *)
module type APIext = sig
  include API
  (* Also identifies the problem we are trying to solve as an input assignment *)
  val problem    : WB.DS.Assign.t
  (* Expected answer if the above if known *)
  val expected   : bool option

  (* Below:
     checks a CDSAT definitive answer against the problem we are supposed to solve *)
  type answer = private
              | UNSAT of unsat WB.t
              | SAT of WB.DS.Assign.t
              | NotAnsweringProblem

  val answer : (unsat WB.t, WB.sat_ans) sum -> answer
end

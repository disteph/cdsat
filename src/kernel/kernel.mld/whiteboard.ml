(*********************)
(* Theory Combinator *)
(*********************)

open Top
open Basic
open Interfaces_basic
open Variables
open Messages
open Theories.Register
open Specs
       
(* This is the module type that we are going to produce at the end of this file *)

module type WhiteBoard = sig

  module DS : sig
    module Term : TermF

    module Value  : sig
      type t [@@deriving eq, ord, hash, show]
    end

    module Assign : sig
      include Collection with type e = Term.t
                          and type t = (Term.t, unit, int, int, unit)
                                         General.Patricia.poly
      val id : t -> int
    end
  end

  module Msg : sig
    type ('sign,'a) t = ('sign,DS.Assign.t,'a) Messages.message
    val pp : Format.formatter -> (_,_)t -> unit
  end

  type 'a t = private WB of unit HandlersMap.t * (unit,'a) Msg.t
  val pp       : Format.formatter -> 'a t -> unit
  val stamp    : 'a Sig.t -> ('a, 'b) Msg.t -> 'b t
  val sat_init : DS.Assign.t -> sat t
  val sat      : sat t -> sat t -> sat t
  val resolve  : straight t -> 'b propa t -> 'b propa t
  val both2straight: ?side:bool -> both t -> unsat t -> straight t
  val curryfy: DS.Assign.t -> unsat t -> straight t
end


module type WhiteBoard_ThModules = sig
  module WB : WhiteBoard
  open WB.DS
  val mdles : (Term.datatype*Value.t*Assign.t) Theories.Register.Modules.t list
end
                           
                           
module Make(Ths: sig val theories: unit HandlersMap.t end)
         (InitState : Combo.State) = struct

  let final_state = 
    let aux (Handlers.Handler hdl) () sofar = Combo.theory_add hdl sofar
    in
    HandlersMap.fold aux Ths.theories (module InitState: Combo.State)

  
  module DS = struct
    module Term   = Terms.Make(FreeVar)(InitState.DT)
    module Value  = InitState.Value
    module Assign = MakePATCollection(Term)
  end

  let conv = {
      Combo.injV = (fun x->x);
      Combo.projV= (fun x->x)
    }

  let th_modules = InitState.modules (fun x -> x) conv (module DS)

  module WB = struct

    module DS = DS
                  
    open DS

    module Msg = struct
      type ('sign,'b) t = ('sign,Assign.t,'b) message
      let pp fmt = print_msg_in_fmt Assign.pp fmt
    end

    type 'a t = WB of unit HandlersMap.t * (unit,'a) Msg.t

    let pp fmt (type a) (WB(hdls,msg) : a t) =
      match msg with
      | Propa _ -> Format.fprintf fmt "%a propagate(s) %a" HandlersMap.pp hdls Msg.pp msg
      | Sat   _ -> Format.fprintf fmt "%a is/are fine with %a" HandlersMap.pp (HandlersMap.diff Ths.theories hdls) Msg.pp msg
                                  

    let sat_init assign = WB(Ths.theories, Messages.sat () assign)
                          
    let sat (WB(rest1, Sat assign1)) (WB(rest2, Sat assign2)) =
      if Assign.equal assign1 assign2
      then WB(HandlersMap.inter rest1 rest2, sat () assign1)
      else failwith "Theories disagree on model"

    let check hdl = if HandlersMap.mem (Handlers.Handler hdl) Ths.theories then ()
                    else failwith "Using a theory that is not allowed"

    let stamp (type b) (Sig.Sig hdl: 'a Sig.t) : ('a,b) Msg.t -> b t = function
      | Propa(assign,o) ->
         check hdl;
         WB(HandlersMap.singleton (Handlers.Handler hdl) (),
            Messages.propa () assign o)
      | Sat assign ->
         WB(HandlersMap.remove (Handlers.Handler hdl) Ths.theories,
            Messages.sat () assign)

    let resolve
          (WB(hdls1,Propa(oldset,Straight newset)))
          (WB(hdls2,Propa(thset,o)))
      =
      WB(HandlersMap.union hdls1 hdls2,
         propa () (Assign.union (Assign.diff thset newset) oldset) o)
        
    (* val both2straight: both t -> unsat t -> straight t *)

    let both2straight
          ?(side=true)
          (WB(hdls1,Propa(oldset,Both(assign1,assign2))))
          (WB(hdls2,Propa(thset,Unsat)))
      = 
      let assign,newset =
        if side then assign1,assign2
        else assign2, assign1
      in
      WB(HandlersMap.union hdls1 hdls2,
         straight () (Assign.union (Assign.diff thset assign) oldset) newset)

    let curryfy assign (WB(hdls,Propa(thset,Unsat))) =
      let assign = Assign.inter assign thset in
      let thset= Assign.diff thset assign in
      let aux term sofar = Term.bC Symbols.Imp [term;sofar] in
      let rhs = Assign.fold aux assign (Term.bC Symbols.False []) in
      WB(hdls,Propa(thset, Straight(Assign.singleton rhs)))

  end
end

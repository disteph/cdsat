(*********************)
(* Theory Combinator *)
(*********************)

open Top
open Basic
open Interfaces_basic
open Variables
open Messages
open Theories_register
open Specs

(* This is the module type that we are going to produce at the end of this file *)

module type WhiteBoard = sig
  module DS : sig
    module Term : TermF
    module TSet : sig
      include Collection with type e = Term.t
                          and type t = (Term.t, unit, int, int, unit)
                                         General.Patricia.poly
      val id : t -> int
    end
  end
  module Msg : sig
    type ('sign,'a) t = ('sign,DS.TSet.t,'a) Messages.message
    val print_in_fmt : Format.formatter -> (_,_)t -> unit
  end
  type 'a t = private WB of unit HandlersMap.t * (unit,'a) Msg.t
  val print_in_fmt : Format.formatter -> 'a t -> unit
  val stamp    : 'a Sig.t -> ('a, 'b) Msg.t -> 'b t
  val sat_init : DS.TSet.t -> sat t
  val sat      : sat t -> sat t -> sat t
  val resolve  : straight t -> 'b propa t -> 'b propa t
  val both2straight: ?side:bool -> both t -> unsat t -> straight t
  val curryfy: DS.TSet.t -> unsat t -> straight t
end

(*********************************************************************)
(* First, we build DS by aggregating a given list of plugins'
   datatypes for representing terms, into one big datatype.

   What we call "a plugin's datatype" is given by the module type
   Top.Specs.DataType
   in which some symbols might not have any interpretation for the
   plugin. 

   We shall quickly convert them in the following module type
   DataType
   where all symbols and all terms can be represented *)
(*********************************************************************)


(* Now we shall organise a traversal of a given list of
   Top.Specs.DataType modules, aggregated all of the datatypes into
   one *)

(* Base case in the traversal *)

let noTheory: (module DataType with type t = unit) =
  (module struct
    type t = unit
    let bC _ _ _ = ()
    let bV _ _   = ()
  end)

(* Incremental case in the traversal *)

let addTheory (type a)(type b)
    (th:(module DataType with type t = a))
    (i :(module DataType with type t = b)) 
    :(module DataType with type t = a*b) =
  let module Th = (val th) in
  let module I = (val i) in
  (module Pairing(Th)(I))

(* Now, this is what we mean by "a list of Top.Specs.DataType
   modules": it's a list, except it's indexed by the aggregated type
   itself, as a product: *)

type _ dataList =
  | NoData : unit dataList
  | ConsData: (module DataType with type t = 'a) * 'b dataList -> ('a*'b) dataList

(* Now, we shall be given a list of the above form, which we shall
   aggregate into datatype, but we shall also have to provide a list
   of projections from the aggregated datatype into each plugin's
   datatype. That list of projections (of the same length of the input
   list) is again an indexed list, of the type below: *)

type (_,_) projList =
  | NoProj  : (_,unit) projList
  | ConsProj: ('t -> 'a) * ('t,'b) projList -> ('t,'a*'b) projList

(* Now we finally organise the traversal: *)

let rec make_datastruct:
type a b. a dataList -> (module DataType with type t = a) * ((b -> a) -> (b,a) projList) 
  = function
  | NoData          -> noTheory, fun _ -> NoProj
  | ConsData(th,l') -> let i, make_pl = make_datastruct l' in
                       addTheory th i, fun f -> ConsProj((fun x -> fst(f x)),make_pl (fun x -> snd(f x)))

(* Now comes the initialisation of the theory combinator, taking as
   input a set of theory handlers and a list of plugins' datatypes. It
   calls the above traversal function, and then produces a
   "whiteboard" (first module type in this file) together with the
   aforementioned projections *)
    
let make (type a)
    (theories: unit HandlersMap.t)
    (l: a dataList)
    : (module WhiteBoard with type DS.Term.datatype = a)
    * (a,a) projList
    =

  (* First we do the traversal *)

  let dt,pl = make_datastruct l in
  let module DT = (val dt) in

  (module struct

    module DS = struct

      module Term = Terms.Make(FreeVar)(DT)
                              
      module TSet = MakePATCollection(
                        struct
                          type t       = Term.t
                          let id       = Terms.id
                          let compare  = Terms.compare
                          let clear    = Term.clear
                          let print_in_fmt = Term.print_in_fmt
                        end)

    end
      
    open DS

    module Msg = struct
      type ('sign,'b) t = ('sign,TSet.t,'b) message
      let print_in_fmt fmt = print_msg_in_fmt TSet.print_in_fmt fmt
    end

    type 'a t = WB of unit HandlersMap.t * (unit,'a) Msg.t

    let print_in_fmt fmt (type a) (WB(hdls,msg) : a t) =
      match msg with
      | Propa _ -> Format.fprintf fmt "%a propagate(s) %a" HandlersMap.print_in_fmt hdls Msg.print_in_fmt msg
      | Sat   _ -> Format.fprintf fmt "%a is/are fine with %a" HandlersMap.print_in_fmt (HandlersMap.diff theories hdls) Msg.print_in_fmt msg
        

    let sat_init tset = WB(theories, Messages.sat () tset)
                          
    let sat (WB(rest1, Sat tset1)) (WB(rest2, Sat tset2)) =
      if TSet.equal tset1 tset2
      then WB(HandlersMap.inter rest1 rest2, sat () tset1)
      else failwith "Theories disagree on model"

    let check hdl = if HandlersMap.mem (Handlers.Handler hdl) theories then ()
      else failwith "Using a theory that is not allowed"

    let stamp (type b) (hdl: 'a Sig.t) : ('a,b) Msg.t -> b t = function
      | Propa(tset,o) ->
         check hdl;
         WB(HandlersMap.singleton (Handlers.Handler hdl) (),
            Messages.propa () tset o)
      | Sat tset ->
         WB(HandlersMap.remove (Handlers.Handler hdl) theories,
            Messages.sat () tset)

    let resolve
          (WB(hdls1,Propa(oldset,Straight newset)))
          (WB(hdls2,Propa(thset,o)))
      =
      WB(HandlersMap.union hdls1 hdls2,
         propa () (TSet.union (TSet.diff thset newset) oldset) o)
              
    (* val both2straight: both t -> unsat t -> straight t *)

    let both2straight
          ?(side=true)
          (WB(hdls1,Propa(oldset,Both(tset1,tset2))))
          (WB(hdls2,Propa(thset,Unsat)))
      = 
      let tset,newset =
        if side then tset1,tset2
        else tset2, tset1
      in
      WB(HandlersMap.union hdls1 hdls2,
         straight () (TSet.union (TSet.diff thset tset) oldset) newset)

    let curryfy tset (WB(hdls,Propa(thset,Unsat))) =
      let tset = TSet.inter tset thset in
      let thset= TSet.diff thset tset in
      let aux term sofar = Term.bC Symbols.Imp [term;sofar] in
      let rhs = TSet.fold aux tset (Term.bC Symbols.False []) in
      WB(hdls,Propa(thset, Straight(TSet.singleton rhs)))

   end),
  pl (fun x -> x)


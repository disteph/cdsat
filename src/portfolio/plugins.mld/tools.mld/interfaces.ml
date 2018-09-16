open Async

open General.Sums
       
open Kernel
open Top
open Messages
open Terms
open Values
open Sassigns
open Theories.Theory
open Theories.Register
       
module type Extra = sig

  type _ t      (* WB's main type *)

  (* Useful type abbreviations *)
  type vvalue = Value.t values
         
  type ack = private AckL
  type say = private MsgL

  type _ answer = Ack :            ack answer
                | Say : _ t     -> say answer
                | Try : (SAssign.t*float) list -> say answer
    
  type msg2pl = Msg : Handlers.t option * _ answer * int -> msg2pl

  type regular = private RegularL
  type egraph  = private EGraphL

  type 'a ports = {
      reader : 'a msg2th Pipe.Reader.t;
      writer : msg2pl Pipe.Writer.t;
      eports : 'a eports
    }
   and _ eports =
     | EPorts      : egraph eports
     | RegularPorts: (Term.t,vvalue) sum Pipe.Writer.t -> regular eports
   and _ msg2th =
     | MsgStraight : SAssign.t*int         ->  _ msg2th
     | MsgSharing  : TSet.t*int            ->  _ msg2th
     | MsgPropose  : Term.t option*int*int ->  _ msg2th
     | MsgSpawn    : 'a ports            -> 'a msg2th
     | Infos       : (Term.t,vvalue) sum*Term.t*CValue.t*(unit->CValue.t list) -> regular msg2th
     | TheoryAsk   : (regular msg2th Pipe.Writer.t) * ((Term.t,vvalue) sum) -> egraph msg2th
     | KillYourself: unsat t * SAssign.t * SAssign.t option -> _ msg2th

  val pp_msg2th : _ msg2th Format.printer

end

module type WhiteBoardExt = sig
  include Combo.WhiteBoard
  include Extra with type 'a t := 'a t
end

module type WhiteBoard4Master = sig

  module WBE : WhiteBoardExt

  module H : sig
    type t
    (* Spawning a hub *)
    val spawn     : t -> t Deferred.t
    (* Killing the communication pipes between master and slaves *)
    val kill      : t -> unit
    (* Sending a message to all slaves *)
    val broadcast : t -> SAssign.t -> chrono:int -> unit Deferred.t
    (* Sending new shared terms to all slaves *)
    val share     : t -> TSet.t -> chrono:int -> unit Deferred.t
    (* Telling all slaves to kill themselves *)
    val suicide   : t -> unsat WBE.t -> SAssign.t -> SAssign.t option ->
                    unit Deferred.t
    (* Calling for decision proposals *)
    val propose   : t -> ?term:Term.t -> int -> chrono:int -> unit Deferred.t
    (* The pipe reader on which the master thread must read *)
    val reader    : t -> WBE.msg2pl Pipe.Reader.t
  end

  val theories_fold : (Handlers.t -> 'a -> 'a) -> 'a -> 'a

end

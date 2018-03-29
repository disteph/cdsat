open Async

open General.Sums
       
open Kernel
open Top
open Messages
open Specs
open Sassigns
open Theories.Register
       
module type Extra = sig

  (* The data-structures defined by the combination *)
  type termdata
  type value
  type assign
  type tset
  type cval
  type _ t      (* WB's main type *)

  (* Useful type abbreviations *)
  type datatypes = termdata*value*assign*tset
  type term = termdata termF
  type vvalue = value values
  type nonrec sassign = (term,value)sassign
                        
         
  type ack = private AckL
  type say = private MsgL

  type _ answer = Ack :            ack answer
                | Say : _ t     -> say answer
                | Try : (sassign*float) list -> say answer
    
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
     | RegularPorts: (term,vvalue) sum Pipe.Writer.t -> regular eports
   and _ msg2th =
     | MsgStraight : sassign*int         ->  _ msg2th
     | MsgSharing  : tset*int            ->  _ msg2th
     | MsgPropose  : term option*int*int ->  _ msg2th
     | MsgSpawn    : 'a ports            -> 'a msg2th
     | Infos       : (term,vvalue) sum*term*cval*(unit->cval list) -> regular msg2th
     | TheoryAsk   : (regular msg2th Pipe.Writer.t)
                     * ((term,vvalue) sum)
                     -> egraph msg2th
     | KillYourself: unsat t * sassign * sassign option -> _ msg2th

  val pp_msg2th : Format.formatter -> _ msg2th -> unit
                                                            
  type 'a ioutput       = ('a, datatypes) output
  type 'a islot_machine = ('a, datatypes) slot_machine
  type isslot_machine   = datatypes PluginsTh.PluginTh.sslot_machine
end

module type WhiteBoardExt = sig
  include Export.WhiteBoard
  open DS
  include Extra with type 'a t := 'a t
                 and type termdata := Term.datatype
                 and type value := Value.t
                 and type assign := Assign.t
                 and type tset := TSet.t
                 and type cval := CValue.t
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
    val broadcast : t -> WBE.sassign -> chrono:int -> unit Deferred.t
    (* Sending new shared terms to all slaves *)
    val share     : t -> WBE.DS.TSet.t -> chrono:int -> unit Deferred.t
    (* Telling all slaves to kill themselves *)
    val suicide   : t -> unsat WBE.t -> WBE.sassign -> WBE.sassign option ->
                    unit Deferred.t
    (* The pipe reader on which the master thread must read *)
    val reader    : t -> WBE.msg2pl Pipe.Reader.t
  end

  val theories_fold : (Handlers.t -> 'a -> 'a) -> 'a -> 'a

end

open Async

open General.Sums
       
open Kernel
open Top
open Messages
open Specs
open Theories.Register
       
module type Extra = sig

  (* The 3 data-structures defined by the combination *)
  type termdata
  type value
  type cval
  type assign
  type _ t (* WB messages *)

  (* Useful type abbreviations *)
  type datatypes = termdata*value*assign
  type term = termdata termF
  type vvalue = value Values.t
  type sassign = term*vvalue
                        
         
  type ack = private AckL
  type say = private MsgL

  type _ answer = Ack :            ack answer
                | Say : _ t     -> say answer
                | Try : sassign -> say answer
    
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
     | MsgBranch   : 'a ports * 'a ports -> 'a msg2th
     | Infos       : (term,vvalue) sum*term*cval*(unit->cval list) -> regular msg2th
     | TheoryAsk   : (regular msg2th Pipe.Writer.t)
                     * ((term,vvalue) sum)
                     -> egraph msg2th
     | KillYourself: unsat t * sassign -> _ msg2th

  val pp_msg2th : Format.formatter -> _ msg2th -> unit
                                                            
  type 'a ioutput       = ('a, datatypes) output
  type 'a islot_machine = ('a, datatypes) slot_machine
  type isslot_machine   = datatypes PluginsTh.PluginTh.sslot_machine
end

module type WhiteBoardExt = sig
  include Export.WhiteBoard
  open DS
  include Extra with type 'a t := 'a t and type termdata := Term.datatype and type value := Value.t and type assign := Assign.t and type cval := CValue.t
end

module type WhiteBoard4Master = sig

  module WBE : WhiteBoardExt

  module H : sig
    type t
    (* Cloning a hub into 2 hubs *)
    val clone     : t -> (t*t) Deferred.t
    (* Killing the communication pipes between master and slaves *)
    val kill      : t -> unit
    (* Sending a message to all slaves *)
    val broadcast : t -> WBE.sassign -> chrono:int -> unit Deferred.t
    (* Telling all slaves to kill themselves *)
    val suicide   : t -> unsat WBE.t -> WBE.sassign -> unit Deferred.t
    (* The pipe reader on which the master thread must read *)
    val reader    : t -> WBE.msg2pl Pipe.Reader.t
  end

  val theories_fold : (Handlers.t -> 'a -> 'a) -> 'a -> 'a

end

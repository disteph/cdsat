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

(* Useful type abbreviations *)
type vvalue = Value.t values

type ack = private AckL
type say = private MsgL
type regular = private RegularL
type egraph  = private EGraphL

type 'a level = Irrelevant : sat level | Level : int -> _ propa level

module type Extra = sig

  type _ t (* WB's main type *)

  type _ answer = Ack  :                           ack answer
                | Say  : _ t                    -> say answer
                | Quid : Term.t                 -> say answer
                | Try  : (SAssign.t*float) list -> say answer
    
  type msg2pl = Msg : { handler : Handlers.t option;
                        answer  : _ answer;
                        chrono  : int } -> msg2pl
      
  type 'a ports = {
      reader : 'a msg2th Pipe.Reader.t;
      writer : msg2pl Pipe.Writer.t;
      eports : 'a eports
    }
   and _ eports =
     | EPorts      : egraph eports
     | RegularPorts: (Term.t,vvalue) sum Pipe.Writer.t -> regular eports
   and _ msg2th =
     | MsgStraight : { sassign : SAssign.t;
                       level : int;
                       chrono : int}                         ->  _ msg2th
     | MsgSharing  : { tset : TSet.t;
                       chrono : int}                         ->  _ msg2th
     | MsgPropose  : { whatabout : Term.t option;
                       howmany   : int;
                       chrono    : int }                     ->  _ msg2th
     | MsgSpawn    : 'a ports                                -> 'a msg2th
     | Infos       : { node        : (Term.t,vvalue) sum;
                       normal_form : Term.t;
                       values      : CValue.t;
                       forbidden   : unit -> CValue.t list } -> regular msg2th
     | TheoryAsk   : { reply_to : regular msg2th Pipe.Writer.t;
                       node : (Term.t,vvalue) sum }          -> egraph msg2th
     | KillYourself: { conflict : unsat t;
                       watch1   : SAssign.t;
                       watch2   : SAssign.t option }         -> _ msg2th

  val pp_msg2th : _ msg2th Format.printer

end

module type S = sig
  include Combo.WhiteBoard
  include Extra with type 'a t := 'a t
end

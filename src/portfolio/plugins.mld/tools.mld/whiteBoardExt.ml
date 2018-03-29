open Async

open General.Sums
open Kernel
open Top.Messages
open Top.Specs
open Top.Sassigns
open Theories.Register
open Interfaces
                             
module Make(WB: Export.WhiteBoard) = struct

  open WB
  open DS
  type datatypes = Term.datatype*Value.t*Assign.t*TSet.t
  type term      = Term.t
  type vvalue    = Value.t values
  type cval      = CValue.t
  type nonrec sassign = sassign
                                  
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
     | MsgStraight : sassign*int         -> _ msg2th
     | MsgSharing  : TSet.t*int          -> _ msg2th
     | MsgSpawn    : 'a ports            -> 'a msg2th
     | Infos       : (term,vvalue) sum*term*cval*(unit->cval list) -> regular msg2th
     | TheoryAsk   : (regular msg2th Pipe.Writer.t)
                     * ((term,vvalue) sum)
                     -> egraph msg2th
     | KillYourself: unsat t * sassign * sassign option -> _ msg2th

  let pp_tv = General.Sums.pp_sum Term.pp (pp_values Value.pp)
                                                             
  let pp_msg2th fmt (type a) : a msg2th -> unit = function
    | MsgStraight(assign,chrono)
      -> Format.fprintf fmt "MsgStraight_%i %a" chrono pp_sassign assign
    | MsgSharing(tset,chrono)
      -> Format.fprintf fmt "MsgSharing_%i %a" chrono TSet.pp tset
    | MsgSpawn _
      -> Format.fprintf fmt "MsgSpawn"
    | Infos(tv,nf,cval,distinct)
      -> Format.fprintf fmt "Infos for %a:\nNormal form is %a\nValues are %a\nDistinct values are %a"
           pp_tv tv Term.pp nf CValue.pp cval (List.pp CValue.pp) (distinct())
    | TheoryAsk(_,tv)
      -> Format.fprintf fmt "Asking about %a" pp_tv tv
    | KillYourself(_,_,_)
      -> Format.fprintf fmt "KillYourself"

  type 'a ioutput       = ('a, datatypes) output
  type 'a islot_machine = ('a, datatypes) slot_machine
  type isslot_machine   = datatypes PluginsTh.PluginTh.sslot_machine
end

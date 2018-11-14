open Async

open General.Sums
open Kernel
open Top
open Terms
open Values
open Sassigns
open Messages

open Theories.Theory
open Theories.Register
open Theories.Eq.MyTheory

open PluginsTh
open PluginsTh.Tools

include WhiteBoardExt_sig
                             
module Make(WB: Combo.WhiteBoard) = struct

  open WB
  
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
     | RegularPorts: node Pipe.Writer.t -> regular eports
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
     | KillYourself: { conflict : unsat t;
                       watch1   : SAssign.t;
                       watch2   : SAssign.t option }         -> _ msg2th
     | TheoryAsk   : { reply_to : regular msg2th Pipe.Writer.t;
                       node     : node }                     -> egraph msg2th
     | Infos       : { node        : node;
                       normal_form : Term.t;
                       values      : CValue.t;
                       forbidden   : unit -> CValue.t list } -> regular msg2th
     | WatchThis   : { reply_to : regular msg2th Pipe.Writer.t;
                       constr   : Constraint.t }          -> egraph msg2th
     | WatchFailed : Constraint.t                         -> regular msg2th

  let pp_tv = General.Sums.pp_sum Term.pp (pp_values Value.pp)
                                                             
  let pp_msg2th fmt (type a) : a msg2th -> unit = function
    | MsgStraight{sassign; level; chrono}
      -> Format.fprintf fmt "MsgStraight_%i@%i %a" chrono level SAssign.pp sassign
    | MsgSharing{tset; chrono}
      -> Format.fprintf fmt "MsgSharing_%i %a" chrono TSet.pp tset
    | MsgPropose{whatabout = Some term; howmany; chrono}
      -> Format.fprintf fmt "MsgPropose_%i %a %i" chrono Term.pp term howmany
    | MsgPropose{whatabout = None; howmany; chrono}
      -> Format.fprintf fmt "MsgPropose_%i %i" chrono howmany
    | MsgSpawn _
      -> Format.fprintf fmt "MsgSpawn"
    | Infos{ node; normal_form; values; forbidden }
      -> Format.fprintf fmt "Infos for %a:\nNormal form is %a\nValues are %a\nDistinct values are %a"
           pp_tv node Term.pp normal_form CValue.pp values (List.pp CValue.pp) (forbidden())
    | TheoryAsk{ node }
      -> Format.fprintf fmt "Asking about %a" pp_tv node
    | KillYourself{ conflict }
      -> Format.fprintf fmt "KillYourself"
    | WatchThis { constr }
      -> Format.fprintf fmt "WatchThis(%a)" Constraint.pp constr
    | WatchFailed constr
      -> Format.fprintf fmt "WatchFailed(%a)" Constraint.pp constr

end

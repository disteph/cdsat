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
open Interfaces
                             
module Make(WB: Combo.WhiteBoard) = struct

  open WB
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
     | Infos       : (Term.t,vvalue) sum*Term.t*CValue.t*(unit->CValue.t list)
       -> regular msg2th
     | TheoryAsk   : (regular msg2th Pipe.Writer.t) * ((Term.t,vvalue) sum) -> egraph msg2th
     | KillYourself: unsat t * SAssign.t * SAssign.t option -> _ msg2th

  let pp_tv = General.Sums.pp_sum Term.pp (pp_values Value.pp)
                                                             
  let pp_msg2th fmt (type a) : a msg2th -> unit = function
    | MsgStraight(assign,chrono)
      -> Format.fprintf fmt "MsgStraight_%i %a" chrono SAssign.pp assign
    | MsgSharing(tset,chrono)
      -> Format.fprintf fmt "MsgSharing_%i %a" chrono TSet.pp tset
    | MsgPropose(_,number,chrono)
      -> Format.fprintf fmt "MsgPropose_%i %i" chrono number
    | MsgSpawn _
      -> Format.fprintf fmt "MsgSpawn"
    | Infos(tv,nf,cval,distinct)
      -> Format.fprintf fmt "Infos for %a:\nNormal form is %a\nValues are %a\nDistinct values are %a"
           pp_tv tv Term.pp nf CValue.pp cval (List.pp CValue.pp) (distinct())
    | TheoryAsk(_,tv)
      -> Format.fprintf fmt "Asking about %a" pp_tv tv
    | KillYourself(_,_,_)
      -> Format.fprintf fmt "KillYourself"

end

open Kernel
open Top.Messages
open Interfaces
open Combo

type 'tset eat_this = EatThis: ('a,'tset,thStraight) thsays -> 'tset eat_this

(* type (_,_) plsays = private *)
(* | PlThProvable: *)
(*     ('sign,'tset,thProvable) thsays  *)
(*   -> ('sign,'tset) plsays *)
(* | PlThNotProvable: *)
(*       ('sign,'tset,thNotProvable) thsays * ('sign,'tset) task *)
(*     -> ('sign,'tset) plsays *)
(* | PlThStraight: *)
(*         ('sign,'tset,thStraight) thsays * ('sign,'tset) task *)
(*       -> ('sign,'tset) plsays *)
(* | PlThAnd: *)
(*           ('sign,'tset,thAnd) thsays * ('sign,'tset) task * ('sign,'tset) task *)
(*         -> ('sign,'tset) plsays *)
(* | PlThOr: *)
(*             ('sign,'tset,thOr) thsays * ('sign,'tset) task * ('sign,'tset) task *)
(*           -> ('sign,'tset) plsays *)
(* and ('sign,'tset) task = 'tset option -> ('sign,'tset) plsays *)


module type Type = sig

  type sign
  type tset

  type slot_machine = SM:
    (sign,tset,'msg) thsays option
    *(tset eat_this option -> slot_machine)
    -> slot_machine

  val search: tset -> slot_machine

end

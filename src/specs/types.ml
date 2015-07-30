open Kernel
open Top.Messages
open Theories_register
open Combo

type _ slot_machine = SM:
  ('tset,'msg) thanswer option
  *(('tset,thStraight) thanswer option -> 'tset slot_machine)
  -> 'tset slot_machine

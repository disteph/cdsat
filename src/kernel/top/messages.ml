(*****************)
(* Message types *)
(*****************)

open Format

open Interfaces_basic
open Basic

type thProvable = private TProvable
type thNotProvable = private TNotProvable
type thStraight = private TStraight
type thAnd = private TAnd
type thOr = private TOr

type (_,_,_) thsays =
| ThProvable   : 'tset -> (_,'tset,thProvable) thsays
| ThNotProvable: 'tset -> (_,'tset,thNotProvable) thsays
| ThStraight: 'tset*'tset   -> (_,'tset,thStraight) thsays
| ThAnd : 'tset*'tset*'tset -> (_,'tset,thAnd) thsays
| ThOr  : 'tset*'tset*'tset -> (_,'tset,thOr) thsays


let thProvable _ tset    = ThProvable tset
let thNotProvable _ tset = ThNotProvable tset
let thStraight _ tset old  = ThStraight(tset,old)
let thAnd _ tset tset' old = ThAnd(tset,tset',old)
let thOr _ tset tset' old  = ThOr(tset,tset',old)

let print_msg_in_fmt tsetprint fmt (type a): (_,_,a)thsays -> unit = function
  | ThProvable tset
    -> fprintf fmt "ThProvable(%a)" tsetprint tset
  | ThNotProvable tset
    -> fprintf fmt "ThNotProvable(%a)" tsetprint tset
  | ThStraight(newset,oldset)
    -> fprintf fmt "ThStraight(%a, %a)" tsetprint newset tsetprint oldset
  | ThAnd(newset1,newset2,oldset)
    -> fprintf fmt "ThAnd(%a, %a, %a)" tsetprint newset1 tsetprint newset2 tsetprint oldset
  | ThOr(newset1,newset2,oldset)
    -> fprintf fmt "ThOr(%a, %a, %a)" tsetprint newset1 tsetprint newset2 tsetprint oldset

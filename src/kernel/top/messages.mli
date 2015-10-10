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

type (_,_,_) thsays = private
| ThProvable   : 'tset -> (_,'tset,thProvable) thsays
| ThNotProvable: 'tset -> (_,'tset,thNotProvable) thsays
| ThStraight: 'tset*'tset   -> (_,'tset,thStraight) thsays
| ThAnd : 'tset*'tset*'tset -> (_,'tset,thAnd) thsays
| ThOr  : 'tset*'tset*'tset -> (_,'tset,thOr) thsays

val thProvable   : 'sign -> 'tset -> ('sign,'tset,thProvable) thsays
val thNotProvable: 'sign -> 'tset -> ('sign,'tset,thNotProvable) thsays
val thStraight: 'sign -> 'tset -> 'tset                      -> ('sign,'tset,thStraight) thsays
val thAnd : 'sign -> 'tset -> 'tset -> 'tset -> ('sign,'tset,thAnd) thsays
val thOr  : 'sign -> 'tset -> 'tset -> 'tset -> ('sign,'tset,thOr) thsays

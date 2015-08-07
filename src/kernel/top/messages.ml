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
| ThStraight: 'tset*('tset->'tset)          -> (_,'tset,thStraight) thsays
| ThAnd : 'tset*'tset*('tset->'tset->'tset) -> (_,'tset,thAnd) thsays
| ThOr  : 'tset*'tset*(bool->'tset->'tset)  -> (_,'tset,thOr) thsays


let thProvable _ tset    = ThProvable tset
let thNotProvable _ tset = ThNotProvable tset
let thStraight _ tset f  = ThStraight(tset,f)
let thAnd _ tset tset' f = ThAnd(tset,tset',f)
let thOr _ tset tset' f  = ThOr(tset,tset',f)

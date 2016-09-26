(*******************************************************************)
(* API for Memoisation of results.
   
   Now we provide tools for memoising the proof-search function.
   Provided that plugin gives a bit more information about its data
   structures, module Memo provides memoisation handling
   functions, and one function to clear the memoisation table.

   A plugin can store the result of a search with f:tomem

   For the plugin to use these functions, it must construct a
   memoisation table by providing more structure than just F, FSet,
   ASet, i.e. providing small extensions of FSet and ASet *)
(*******************************************************************)

open Kernel.Prop.Interfaces_plugin
open SetInterface

module Make
  (FE: FrontEndType)
  (FS: CollectImplemExt with type e = FE.FSet.e and type t=FE.FSet.ps)
  (AS: CollectImplemExt with type e = FE.ASet.e and type t=FE.ASet.ps)
  : sig
    open FE
    val tomem          : seqU answer -> (AS.t*FS.t) option

    val get_usage_stats4provable : seqU answer->int 
    val reset_stats4provable     : seqU answer->unit
    val search4provableNact    : seqU seq-> ('a address*'a address) -> 'a alt_action->'a alt_action
    val search4notprovableNact : seqU seq->(unit->'a focusCoin)->'a focusCoin

    val report         : unit->unit
    val clear          : unit->unit
    val size           : unit->int
  end

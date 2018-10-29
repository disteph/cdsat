open Async

open Kernel.Theories.Register

include module type of Hub_sig
  
module Make(WB: WhiteBoardExt.S) : sig
  include S with type 'a wb  := 'a WB.t
             and type msg2pl := WB.msg2pl

  open WhiteBoardExt

  (* Constructing a hub *)
  val make :
    egraph_init:(egraph WB.ports -> unit Deferred.t)
    -> memo_init:(regular WB.ports -> unit Deferred.t)
    -> other_init:(regular WB.ports -> unit Deferred.t) HandlersMap.t 
    ->  unit Deferred.t * t
end

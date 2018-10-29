(*********************************************************************)
(* Main plugin, implementing the combination of decision procedures
   with concurrency, as provided by Jane Street's Async library.

   This is a master-slaves architecture.

   Each slave thread runs the code written in worker.ml, controlling
   the (purely sequential) execution of a decision procedure, and
   exchanging messages with the master thread, whose code is below.  *)
(*********************************************************************)

open Kernel.Theories.Theory

open Tools

module type WhiteBoard4Master = sig

  module WBE : WhiteBoardExt.S

  module H : Hub.S with type 'a wb  := 'a WBE.t
                    and type msg2pl := WBE.msg2pl

  val theories_fold : (Handlers.t -> 'a -> 'a) -> 'a -> 'a

end

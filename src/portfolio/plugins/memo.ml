open Async.Std

open Kernel

open Combo
open Top
open HCons
open Messages
open Specs

open General
open SetConstructions

open PluginsTh_tools

open LoadPluginsTh

type sign = unit
              
module Make(WB : WhiteBoardExt.Type) = struct

  open WB
  open DS

  (* HConsed version of WB's messages *)
  module M = struct
    type (_,'a) t = 'a WB.t
    let equal _ _ (WB(hdls1,msg1)) (WB(hdls2,msg2)) =
      failwith "TODO"
    let hash _ _ (WB(hdls,msg)) =
      failwith "TODO"
  end
  module H = MakePoly(M)

  module Bogus = struct
    type t = unsat
    let equal _ _ = true
    let hash _ = 0
  end
  module H' = H.Init(NoBackIndex)(Bogus)
               
  module Config = struct
    
    (*******************************************************************)
    (* These are the ingredients to feed the 2-watched literals module *)
    (*******************************************************************)

    (* Constraints are unsat messages. *)

    module Constraint =
      (struct
        type t = H'.t*TSet.t
        let id (msg,_) = H.id msg
        let msg (msg,_) = H.reveal msg
        let simpl (_,tset) = tset
        let make msg =
          H'.build msg,
          let WB(_,Propa(tset,_)) = msg in
          tset
        let simplify trail (msg,tset) =
          (msg,TSet.diff tset trail)
      end : sig
        type t
        val id : t -> int
        val msg : t -> unsat WB.t
        val simpl: t -> TSet.t
        val make : unsat WB.t -> t
        val simplify : TSet.t -> t -> t
      end)

    module Var = struct
      include Term
      let compare = Terms.compare
    end

    type fixed = TSet.t
    let init_fixed = TSet.empty

    let simplify = Constraint.simplify
                              
    let pick_another (c : Constraint.t) (var : Var.t) : Var.t option =
      let tset = Constraint.simpl c in
      let tochoose = 
        if TSet.mem var tset
        then TSet.remove var tset
        else tset
      in
      if TSet.is_empty tochoose
      then None
      else let newvar,_ = TSet.next tochoose in Some newvar
  end

  module P = TwoWatchedLits.Make(Config)

  let state = ref P.init

  type state = unit

  let rec machine state  =
    (module struct

       type newoutput = (sign,TSet.t) output
       type tset = TSet.t

       (* Should send a minimal set af equations *)
       let add = function
         | None -> Output(None, machine state)
         | Some tset -> failwith "todo"

       let normalise _ = failwith "Not a theory with normaliser"

       let clone () = Output(None, machine state)

     end : SlotMachine with type newoutput = (sign,TSet.t) output and type tset = TSet.t)

  let init = machine ()
                  
  let make
        (from_pl : msg2th Pipe.Reader.t)
        (to_pl : msg2pl Pipe.Writer.t)
        (tset : TSet.t)
      : unit Deferred.t
    = return ()

end

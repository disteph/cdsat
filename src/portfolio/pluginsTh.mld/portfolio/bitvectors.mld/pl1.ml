open Kernel

open Top.Terms

open Theories
open Theory
open Theories.Bitvectors

open Datatypes
    
type sign = MyTheory.sign
type api  = (module MyTheory.API)

let hdl = MyTheory.hdl

module Make(W: Writable) = struct

  module Make(K: MyTheory.API) = struct

    type state = {
        kernel : K.state;  (* The state of the kernel *)
        domains: Domain.t; (* The set of variables we could fix, with their domains *)
        silent : bool      (* Whether we have already sent an unsat message *)
      }
                                  
    (* This is the main loop. *)                   
    let rec machine state : sign slot_machine =

      let add a = failwith "TODO"
      in

      let share =
        (* If we have already sent an unsat message, we shut up. *)
        if state.silent then fun _ -> Silence, machine state
        else fun tset -> failwith "TODO"
      in
      
      let clone () = machine state in

      let suicide _ = () in

      let propose ?term _ = [] in

      SlotMachine { add; share; clone; suicide; propose }

    let init = machine { kernel = K.init;
                         domains = Domain.empty;
                         silent = false }

    let clear () = ()

  end
        
  let make (module K: MyTheory.API)
    = let module Made = Make(K) in
      { PluginTh.init = Made.init;
        PluginTh.clear = Made.clear }

end

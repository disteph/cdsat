open General
open Sums
open Patricia
open Patricia_tools

open Kernel
open Export
open Top.Specs
open Top.Basic
open Top.Sassigns
open Top.Messages
open Theories.Bitvectors
       
type sign = MyTheory.sign

module Make(DS: GlobalImplem) = struct
  open DS
  module Make(K: MyTheory.API with type assign = Assign.t
                               and type termdata= Term.datatype
                               and type tset   = TSet.t )
    = struct

    module DT = Datatypes.Make(DS)(K)
    open DT

    type state = {
        kernel : K.state;  (* The state of the kernel *)
        domains: Domain.t; (* The set of variables we could fix, with their domains *)
        propas : (sign,straight) Msg.t Pqueue.t; (* The propa messages to be send*)
        umsg   : (sign,unsat) Msg.t option;      (* The unsat message to be send*)
        silent : bool      (* Whether we have already sent an unsat message *)
      }


                                  
    (* This is the main loop. *)                   
    let rec machine state : (sign,(Term.datatype*Value.t*Assign.t*TSet.t)) slot_machine =

      let add a = failwith "TODO"
      in

      let share =
        (* If we have already sent an unsat message, we shut up. *)
        if state.silent then fun _ -> Silence, machine state
        else
          fun tset -> failwith "TODO"
      in
      
      let clone () = machine state in

      let suicide _ = ()

      in SlotMachine { add; share; clone; suicide }

    let init = machine { kernel = K.init;
                         domains = Domain.empty;
                         propas = Pqueue.empty();
                         umsg   = None;
                         silent = false }

    let clear () = ()

  end
        
  let make (k: (Term.datatype,Value.t,Assign.t,TSet.t) MyTheory.api)
    = let (module K) = k in
      let module Made = Make(K) in
      { PluginTh.init = Made.init;
        PluginTh.clear = Made.clear }

end

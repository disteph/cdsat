open Kernel
open Top
open Specs
open Messages

open Prop.Literals
open Theories_register
open General
open SetConstructions
open Sums
open PluginsTh_tools

open Bool

type sign = MyTheory.sign
let hdl = Sig.Bool

module ThDS = MyStructures.ThDS

module Make(DS: sig 
  include GTheoryDSType
  val proj: Term.datatype -> ThDS.t
end) =
struct

  open DS

  module Config = struct
    include MyTheory.Make(DS)
    module Var = LitF
  end

  module Propa = Propagate.Make(Config)
    
  type state = {
      treated: TSet.t;
      todo   : Term.t Pqueue.t;
      finalsay: Config.stop option;
      propastate: Propa.t
    }

  let rec machine state =
    (module
       struct
          
         type newoutput = (sign,TSet.t) output
         type tset = TSet.t

         let treated() = state.treated

           
         let add newlits =

           let state =
             match state.finalsay, newlits with
             | Some([],msg), _ -> state
             | _, Some nl when not(TSet.is_empty nl) ->
                {
                  state with
                  treated = TSet.union state.treated nl;
                  todo    = TSet.fold Pqueue.push nl state.todo;
                }
             | _    -> state
           in

           match state.finalsay with

           | Some([],msg) ->
              Output(Some msg,fail_state)

           | Some(msg::l,unsat) ->
              let state = { state with finalsay = Some(l,unsat) } in
              Output(Some msg, machine state)

           | None ->
              
              let rec aux state =
                match Pqueue.pop state.todo with

                | Some(t,todo) ->
                   let c = Config.Constraint.make t in
                   begin
                     match Propa.treat c state.propastate with
                     | A([],msg) ->
                        Output(Some msg,fail_state)
                     | A(msg::l,unsat) ->
                        let state = {state with finalsay = Some(l,unsat) } in
                        Output(Some msg, machine state)
                     | F propastate -> aux { state with propastate = propastate }
                   end

                | None ->

                   let msg,propastate = Propa.extract_msg state.propastate in
                   let next = machine { state with propastate = propastate } in
                   
                   match msg with

                   | Some(Sums.A msg) ->
                      Output(Some msg, next)

                   | Some(Sums.F msg) ->
                      Output(Some msg, next)

                   | None -> 
                      Output(None, next)

              in
              aux state

        let normalise = (fun _ -> failwith "Not a theory with normaliser")

        let clone = (fun () -> Output(None, machine state))

      end : SlotMachine with type newoutput = (sign,TSet.t) output and type tset = TSet.t)

  let init = machine { treated = TSet.empty; todo = Pqueue.empty; finalsay = None; propastate = Propa.init }

end

open Kernel
open Top
open Specs
open Theories_register

open Prop.Literals

open CC

type sign = CC.MyTheory.sign
let hdl = Sig.CC

module ThDS = struct
  type t = LitF.t 
  let bV tag _ = LitF.build(true,tag)
  let bC tag symb l = match symb,l with
    | Symbols.Neg,[a] -> LitF.negation a
    | _,_ ->  bV tag l
end

module Make(DS: sig 
  include GTheoryDSType
  val proj: Term.datatype -> ThDS.t
end) = struct 

  module MyCC = MyTheory.Make(DS)

  let rec init_rec next = 
    let module Next = (val next: MyCC.SlotMachineCC with type t = MyCC.outputCC) in
    let rec state : (sign,DS.TSet.t) slot_machine = (module struct 
      type newoutput = (sign,DS.TSet.t) output
      type tset = DS.TSet.t
      let add = function
        | None -> Output(None,state)
        | Some tset -> 
           match Next.add tset with
           | MyCC.UNSAT msg     -> Output(Some msg, fail_state)
           | MyCC.SAT(msg,cont) -> Output(Some msg, init_rec cont)
      let normalise _ = failwith "Not a theory with normaliser"
      let clone () = Output(None,state)
      let suicide _ = ()
    end)
    in
    state

  let init = init_rec MyCC.init

  let clear () = ()

end

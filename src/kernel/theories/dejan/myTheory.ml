open Top
open Messages
open Specs

type sign = unit

module Make(DS: GTheoryDSType) = struct

  open DS

  let rec state atomN
      = (module struct

        type newoutput = (sign,TSet.t) output
        type tset = TSet.t

        let treated = (fun () -> atomN)

        let add _ = Output(None,state atomN)

        let normalise = (fun _ -> failwith "Not a theory with normaliser")

        let clone = (fun () -> Output(None, state atomN))
      end : SlotMachine with type newoutput = (sign,TSet.t) output and type tset = TSet.t)

  let init = state TSet.empty

end

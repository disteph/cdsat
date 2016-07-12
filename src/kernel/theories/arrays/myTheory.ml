open Top
open Messages
open Specs

type sign = unit

module Make(DS: GTheoryDSType) = struct

  open DS

  let rec state atomN =
    (module struct

      type newoutput = (sign,TSet.t) output
      type tset = TSet.t

      let treated () = atomN

      let add = function
        | None -> Output(None,state atomN)
        | Some tset ->
           let newtreated = TSet.union atomN tset in
           Output(Some(sat () newtreated),state newtreated)

      let normalise _ = failwith "Not a theory with normaliser"

      let clone () = Output(None, state atomN)

    end : SlotMachine with type newoutput = (sign,TSet.t) output and type tset = TSet.t)

  let init = state TSet.empty

end

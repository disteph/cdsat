open Top
open Messages
open Specs

type sign = unit

module Make(DS: GTheoryDSType) = struct

  open DS

  let rec machine atomN
      = (module struct

        type newoutput = (sign,TSet.t) output
        type tset = TSet.t

        let treated () = atomN

        let add = function
          | None -> Output(None, machine atomN)
          | Some newtset ->
             let atomN = DS.TSet.union newtset atomN in
             let tmp = TSet.fold
               (fun l -> function
               | Some _ as ans -> ans
               | None ->
                  let notl = Term.bC Symbols.Neg [l] in
                  if TSet.mem notl atomN 
                  then Some(TSet.add l (TSet.add notl TSet.empty))
                  else None
               )
               atomN
               None in
             match tmp with
             | Some tset -> Output(Some(unsat () tset), fail_state)
             | None -> Output(Some(sat () atomN), machine atomN)

        let normalise _ = failwith "Not a theory with normaliser"

        let clone () = Output(None, machine atomN)

      end : SlotMachine with type newoutput = (sign,TSet.t) output and type tset = TSet.t)

  let init = machine TSet.empty

end

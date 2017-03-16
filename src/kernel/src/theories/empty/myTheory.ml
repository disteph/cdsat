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

        let add = function
          | None -> Output(None, machine atomN)
          | Some newtset ->
             Dump.print ["empty",1] (fun p-> p "Empty adds %a" TSet.print_in_fmt newtset);
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
             | None ->
                Dump.print ["empty",1] (fun p-> p "Empty is fine with %a" TSet.print_in_fmt atomN);
                Output(Some(sat () atomN), machine atomN)

        let normalise _ = failwith "Not a theory with normaliser"

        let clone () = Output(None, machine atomN)

      let suicide _ = ()

      end : SlotMachine with type newoutput = (sign,TSet.t) output and type tset = TSet.t)

  let init = machine TSet.empty
  let clear () = ()
                   
end
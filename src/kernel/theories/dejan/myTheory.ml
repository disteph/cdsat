open Top
open Messages
open Specs

open Algo


module ThDS = struct

  type t = 
    | ArithTerm of num*num
    | Eq of Equation.equation
    | Other

  let bV tag fv = match Variables.FreeVar.get_sort fv with
    | Sorts.Rat -> ArithTerm(num_of_int 1,num_of_int 0)
    | _ -> Other

  let bC tag symb l = match symb,l with
    | Symbols.Neg,[a] -> failwith "TODO"
    | Symbols.Eq Sorts.Rat, l -> failwith "TODO"
    | Symbols.NEq Sorts.Rat, l -> failwith "TODO"
    | Symbols.CstRat n, l -> failwith "TODO"
    | Symbols.Ge, l -> failwith "TODO"
    | Symbols.Le, l -> failwith "TODO"
    | Symbols.Gt, l -> failwith "TODO"
    | Symbols.Lt, l -> failwith "TODO"
    | Symbols.Plus, l -> failwith "TODO"
    | Symbols.Minus, l -> failwith "TODO"
    | Symbols.Times, l -> failwith "TODO"
    | Symbols.Divide, l -> failwith "TODO"
    | Symbols.Op, l -> failwith "TODO"
    | _,_ -> begin
      match Symbols.arity symb with
      | Sorts.Rat, _ -> ArithTerm(num_of_int 1,num_of_int 0)
      | _,_ -> Other
    end

end

type sign = unit

module Make(DS: sig 
  include GTheoryDSType
  val proj: Term.datatype -> ThDS.t
end) = struct

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
           Output(Some(thNotProvable () newtreated),state newtreated)

      let normalise _ = failwith "Not a theory with normaliser"

      let clone () = Output(None, state atomN)

    end : SlotMachine with type newoutput = (sign,TSet.t) output and type tset = TSet.t)

  let init = state TSet.empty

end

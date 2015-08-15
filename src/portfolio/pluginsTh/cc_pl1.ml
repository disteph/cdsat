open Kernel
open Top
open Specs
open Messages
open Theories_register
open Types

open Prop
open Literals

open CC

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

  let sign msg = ThAns(Sig.CC,msg)

  let rec search tset = match MyCC.consistency tset with
    | MyCC.L(msg) -> SM(Some(sign msg), fun _ -> failwith "Are you dumb? I already told you it was provable")
    | MyCC.R(msg) -> SM(Some(sign msg), function
      | None -> search tset
      | Some (ThAns(_,ThStraight(newtset,f))) -> search (DS.TSet.union newtset tset))

end

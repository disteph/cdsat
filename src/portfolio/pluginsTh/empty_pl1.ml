open Kernel
open Top
open Specs
open Messages
open Theories_register
open Types

open Empty

module ThDS = struct
  type t = unit 
  let semantic _ = None
  let leaf _ = ()
end

module Make(DS: sig 
  include GTheoryDSType
  val proj: Term.datatype -> ThDS.t
end) = struct 

  module MyEmpty = Mytheory.Make(DS)

  let sign msg = ThAns(Sig.Empty,msg)

  let rec search tset = match MyEmpty.consistency tset with
    | MyEmpty.L(msg) -> SM(Some(sign msg), fun _ -> failwith "Are you dumb? I already told you it was provable")
    | MyEmpty.R(msg) -> SM(Some(sign msg), function
      | None -> search tset
      | Some (ThAns(_,ThStraight(newtset,f))) -> search (DS.TSet.union newtset tset))

end

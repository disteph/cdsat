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

  let rec search_rec next tset = 
    let module Next = (val next: MyCC.State with type t = MyCC.state) in
    match Next.add tset with
    | MyCC.UNSAT(msg)    -> SM(Some(sign msg), fun _ -> failwith "Are you dumb? I already told you it was provable")
    | MyCC.SAT(msg,cont) -> SM(Some(sign msg), aux cont)
  and aux cont = function
    | None -> SM(None, aux cont)
    | Some(ThAns(_,ThStraight(newtset,f))) -> search_rec cont newtset

  let search = search_rec MyCC.init
      
end

open Kernel
open Basic
open Interfaces_basic
open Interfaces_theory
open Formulae

open Theories_tools
open ForGround
open StandardStruct

let names = ["prop";"empty"]
let sugPlugin = None

include StandardDS(IntSort)

module GConsistency(EAtom: sig
  type t
  val proj: t -> Atom.t
  val negation: t->t
end) =
struct    
  
  module ASet = Sequents.MakeCollectTrusted(
    struct
      type t = EAtom.t
      let id t = Atom.id(EAtom.proj t)
      let clear = Atom.clear
      let compare a b = Atom.compare (EAtom.proj a) (EAtom.proj b)
      let print_in_fmt fmt a = Atom.print_in_fmt fmt (EAtom.proj a)
    end)
    
  let goal_consistency t atomN = 
    if ASet.mem t atomN then Some (ASet.add t ASet.empty)
    else None
      
  let rec consistency atomN =
    ASet.fold 
      (function l -> function
      | Some a -> Some a
      | None   -> 
	(match goal_consistency (EAtom.negation l) atomN with
	| None     -> None
	| Some set -> Some (ASet.add l set)
	)
      )
      atomN
      None
end


module ForParsing(F:Formula.S with type lit = Atom.t) = struct

  include ForParsingWOEx(F)

  let lit(b, f) = F.lit(Atom.build(b, Symbol.User(f,(Sorts.Prop,[])), []))

    (* p(x) \/- !p(x) *)
  let f1() = 
    F.orN(
      lit(true,"p"),
      lit(false,"p")
    )

    (* p(x) \/+ !p(x) *)
  let f2() = 
    F.orP(
      lit(true,"p"),
      lit(false,"p")
    )

    (* !p(x) \/+ p(x) : infinite computation if proof-search is depth-first*)
  let f3() = 
    F.orP(
      lit(false,"p"),
      lit(true,"p")
    )

    (* (a \/- b) \/- (!a /\- !b) *)

  let f4() = 
    F.orN(
      F.orN(
	lit(true,"a"),
	lit(true,"b")
      ),
      F.andN(
	lit(false,"a"),
	lit(false,"b")
      )
    )

    (* (a \/+ b) \/- (!a /\- !b) *)
  let f5() = 
    F.orN(
      F.orP(
	lit(true,"a"),
	lit(true,"b")
      ),
      F.andN(
	lit(false,"a"),
	lit(false,"b")
      )
    )

    (* (!a \/+ !b) not provable - naive algorithm goes into infinite computation *)
  let f6() = 
    F.orP(
      lit(false,"a"), 
      lit(false,"b")
    )

    (* (!a /\- !b) *)

  let f7() =
    F.andN(
      lit(false,"a"), 
      lit(false,"b")
    )

  let f8()=
    F.orN(
      F.orP(
	lit(false,"p"),
	lit(false,"q")
      ),
      F.andP(
	lit(true,"p"),
	lit(true,"q")
      )
    )

    (*
      let f9=
      andN(
      orP(
      lit(false,"p"),
      lit(true,"p")
      ),
      lit(true,"p")
      )
    *)

  let examples =
    [(f1,true);(f2,true);(f3,true);(f4,true);(f5,true);(f6,false);(f7,false);(f8,true)]

end

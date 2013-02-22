open Kernel
open Interfaces
open Formulae

module Atom = Atoms.Atom
  
module DecProc(ASet: CollectImplem with type e = Atom.t)
  = struct    
    
    let goal_consistency atomN t = 
      if ASet.is_in t atomN then Some (ASet.add t ASet.empty)
      else None

    let rec consistency atomN =
      ASet.fold 
	(function l -> function
	   | Some a -> Some a
	   | None   -> 
	       (match goal_consistency atomN (Atom.negation l) with
		  | None -> None
		  | Some set -> Some (ASet.add l set)
	       )
	)
	atomN
	None
  end

module Parser(F:FormulaImplem with type lit = Atom.t)
  = struct

    include MyParser.Generate(F)

    include PrintableFormula(Atom)(F)
    let lit (b, f, tl) = F.build (Lit(Atom.bbuild (b, f, tl)))

    (* p(x) \/- !p(x) *)
    let f1 = 
      orN(
	lit(true,"p",[]),
	lit(false,"p",[])
      )

    (* p(x) \/+ !p(x) *)
    let f2 = 
      orP(
	lit(true,"p",[]),
	lit(false,"p",[])
      )

    (* !p(x) \/+ p(x) : infinite computation if proof-search is depth-first*)
    let f3 = 
      orP(
	lit(false,"p",[]),
	lit(true,"p",[])
      )

    (* (a \/- b) \/- (!a /\- !b) *)

    let f4 = 
      orN(
	orN(
	  lit(true,"a",[]),
	  lit(true,"b",[])
	),
	andN(
	  lit(false,"a",[]),
	  lit(false,"b",[])
	)
      )

    (* (a \/+ b) \/- (!a /\- !b) *)
    let f5 = 
      orN(
	orP(
	  lit(true,"a",[]),
	  lit(true,"b",[])
	),
	andN(
	  lit(false,"a",[]),
	  lit(false,"b",[])
	)
      )

    (* (!a \/+ !b) not provable - naive algorithm goes into infinite computation *)
    let f6 = 
      orP(
	lit(false,"a", []), 
	lit(false,"b", [])
      )

    (* (!a /\- !b) *)

    let f7=
      andN(
	lit(false,"a", []), 
	lit(false,"b", [])
      )

    let f8=
      orN(
	orP(
	  lit(false,"p",[]),
	  lit(false,"q",[])
	),
	andP(
	  lit(true,"p",[]),
	  lit(true,"q",[])
	)
      )

  (*
    let f9=
    andN(
    orP(
    lit(false,"p",[]),
    lit(true,"p",[])
    ),
    lit(true,"p",[])
    )
  *)

    let	examples = f1::f2::f3::f4::f5::f6::f7::f8::[]

  end

(* OLD CODE, useful for first-order 

    let goal_consistency atomN t =
      let (b,p,_) = Atom.reveal t in
      let rec filt_inspect filtered =
	if ASet.is_empty filtered then None
	else
	  let (at,newfiltered) = ASet.next filtered in
	    if Atom.equal at t then Some (ASet.add t ASet.empty)
	    else filt_inspect newfiltered
      in filt_inspect (atomN)

*)

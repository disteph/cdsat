open Kernel
open Formulae

module Atom = Atoms.Atom
  
module DecProc(ASet: Collection.CollectImplem with type e = Atom.t)
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
    let lit (b, f, tl) = F.build (Lit(Atom.bbuild (b, f, tl)))
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

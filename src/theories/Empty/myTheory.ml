open Kernel
open Formulae

module Atom = Atoms.Atom
  
module DecProc(ASet: Collection.CollectImplem with type e = Atom.t)
  = struct    
      
    let goal_consistency atomN t =
      let (b,p,_) = Atom.reveal t in
      let rec filt_inspect filtered =
	if ASet.is_empty filtered then None
	else
	  let (at,newfiltered) = ASet.next filtered in
	    if Atom.equal at t then Some (ASet.add t ASet.empty)
	    else filt_inspect newfiltered
      in filt_inspect (atomN)

    let rec consistency atomN =
      if ASet.is_empty atomN then None
      else
	let (at,newatomN) = ASet.next atomN in
	  match goal_consistency newatomN (Atom.negation at) with
	    | None   -> consistency newatomN
	    | Some a -> Some(ASet.add at a)
  end

module Parser(F:FormulaImplem with type lit = Atom.t)
  = MyParser.Generate(F)

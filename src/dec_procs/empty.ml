open Kernel
open Formulae
open Collection

module Build (ASet: ACollectImplem)
  = struct
    
    let goal_consistency atomN t =
      let (b,p,_) = Atom.reveal t in
      let rec filt_inspect filtered =
	if ASet.is_empty filtered then None
	else
	  let (at,newfiltered) = ASet.next filtered in
	    if Atom.equal at t then Some (ASet.add t ASet.empty)
	    else filt_inspect newfiltered
      in filt_inspect (ASet.filter b p atomN)

    let rec consistency atomN =
      if ASet.is_empty atomN then None
      else
	let (at,newatomN) = ASet.next atomN in
	  match goal_consistency newatomN (Atom.negation at) with
	    | None   -> consistency newatomN
	    | Some a -> Some(ASet.add at a)

  end

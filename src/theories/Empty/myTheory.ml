open Kernel
open Interfaces_I
open Formulae

module Sig    = ThSig_register.PropSig
module Atom   = MyAtom.Atom

let sugPlugin = None

module Consistency(ASet: CollectImplem with type e = Atom.t)
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
	  | None     -> None
	  | Some set -> Some (ASet.add l set)
	  )
	)
	atomN
	None
  end

module Structure(F:Formulae.FormulaType with type lit = Atom.t)
  = struct

    open Theories

    type t = F.t

    let lit (b, f, tl) = F.lit(Atom.bbuild (b, f, tl))

    let st = 
      { sigsymb_i 
	= (fun symb ->
	     let module TT = ThDecProc_tools.PropStructure(F) in
	       TT.symb_i symb);
	decsymb_i = 
	  (function
	  | `Prop -> fun (var:string) l ->
            if l <> [] 
            then raise (ModelError ("ModelError: Variable"^var^"shouldn't have any argument"))  
	    else lit(true,var,[])
	  | _     -> fun (var:string) -> raise (ModelError ("ModelError: Variable "^var^" not of expected type `Prop")));

        boundsymb_i = (fun db so -> raise (ModelError ("ModelError: cannot treat bound variables")));
        quantif_i = (fun db so sf -> raise (ModelError ("ModelError: cannot treat quantifiers")))

      }

    let toform a = a


    (* p(x) \/- !p(x) *)
    let f1() = 
      F.orN(
	lit(true,"p",[]),
	lit(false,"p",[])
      )

    (* p(x) \/+ !p(x) *)
    let f2() = 
      F.orP(
	lit(true,"p",[]),
	lit(false,"p",[])
      )

    (* !p(x) \/+ p(x) : infinite computation if proof-search is depth-first*)
    let f3() = 
      F.orP(
	lit(false,"p",[]),
	lit(true,"p",[])
      )

    (* (a \/- b) \/- (!a /\- !b) *)

    let f4() = 
      F.orN(
	F.orN(
	  lit(true,"a",[]),
	  lit(true,"b",[])
	),
	F.andN(
	  lit(false,"a",[]),
	  lit(false,"b",[])
	)
      )

    (* (a \/+ b) \/- (!a /\- !b) *)
    let f5() = 
      F.orN(
	F.orP(
	  lit(true,"a",[]),
	  lit(true,"b",[])
	),
	F.andN(
	  lit(false,"a",[]),
	  lit(false,"b",[])
	)
      )

    (* (!a \/+ !b) not provable - naive algorithm goes into infinite computation *)
    let f6() = 
      F.orP(
	lit(false,"a", []), 
	lit(false,"b", [])
      )

    (* (!a /\- !b) *)

    let f7() =
      F.andN(
	lit(false,"a", []), 
	lit(false,"b", [])
      )

    let f8()=
      F.orN(
	F.orP(
	  lit(false,"p",[]),
	  lit(false,"q",[])
	),
	F.andP(
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

    let	examples =
      [(f1,true);(f2,true);(f3,true);(f4,true);(f5,true);(f6,false);(f7,false);(f8,true)]

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

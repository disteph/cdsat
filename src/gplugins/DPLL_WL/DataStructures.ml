(* ******************************************* *)
(* Implementation of sets of atoms for DPLL,
   Implementation of formulae for DPLL,
   Implementation of sets of formulae for DPLL *)
(* ******************************************* *)

open Lib
open Kernel

open Interfaces
open Formulae
open Sums
open SetConstructions
open Common.SetInterface


module Generate(Atom:AtomType) = struct

  (* **************************************** *)
  (* Implementation of sets of atoms for DPLL *)

  module ASet = struct

    module AtSet = Common.Patricia_ext.MyPatriciaCollectImplem(Atom)

    type e              = Atom.t
    type t              = AtSet.t*(e option)
    let hash(a,_)       = AtSet.hash a
    let equal(a,_)(a',_)= AtSet.equal a a'
    let empty           = (AtSet.empty, None)
    let is_empty(a,_)   = AtSet.is_empty a
    let union(a,_)(a',_)= (AtSet.union a a',None)
    let inter(a,_)(a',_)= (AtSet.inter a a',None)
    let subset(a,_)(a',_)= AtSet.subset a a'
    let is_in l (t,_)   = AtSet.is_in l t
    let add l (h,_)     = (AtSet.add l h,Some l)
    let remove l (h,_)  = (AtSet.remove l h, None)
    let next (t1,a)     = 
      let (l,t2) = AtSet.next t1 in
	(l, (t2,None))
    let fold f (a,_)    = AtSet.fold f a
    let toString (h,_)  = AtSet.toString h 
      (*    let toString (h,_)= SS.toString (Some((fun i->string_of_int i^"|"),(fun i->string_of_int i^"|"))) (fun (k,l)->("F"^string_of_int(AtSet.SS.id l)^AtSet.toString l)) h *)
    let diff (t1,_)(t2,_) = (AtSet.diff t1 t2,None)
    let compare(a,_)(a',_)    = AtSet.compare a a'
    let compareE              = AtSet.compareE
    let first_diff(a,_)(a',_) = AtSet.first_diff a a'
    (* let first_diff(a,_)(a',_) = match AtSet.first_diff a a' with  *)
    (*   |(Some c,g) -> print_endline("first_diff "^AtSet.toString a^"   "^AtSet.toString a'^"   "^Atom.toString c);(Some c,g)  *)
    (*   |(c,g)->(c,g)  *)
    let sub alm (s1,f) (s2,g) limit = AtSet.sub alm s1 s2 limit
    let choose (t,_)    = AtSet.choose t
    let clear ()        = AtSet.clear()
    let id (a,_)        = AtSet.id a
    let latest (_,b)    = b
    let cardinal (s,_)  = AtSet.cardinal s
    let negations (s,_) = AtSet.fold (fun k accu -> add (Atom.negation k) accu) s empty

  end


  (* *********************************** *)
  (* Implementation of formulae for DPLL *)

  module F = struct

    type lit = Atom.t

    type tt = {reveal: (tt,Atom.t) form; id:int; aset: ASet.t option}

    let id f   = f.id
    let aset f = f.aset
      
    (* HashedType for formulae *)

    module MySmartFormulaImplemPrimitive = 
      (struct
	 type t = tt
	 let equal t1 t2 =
	   match t1.reveal,t2.reveal with
	     | Lit l1, Lit l2             -> l1==l2
	     | AndP (x1,x2), AndP (y1,y2) -> x1==y1 && x2==y2
	     | OrP (x1,x2), OrP (y1,y2)   -> x1==y1 && x2==y2
	     | AndN (x1,x2), AndN (y1,y2) -> x1==y1 && x2==y2
	     | OrN (x1,x2), OrN (y1,y2)   -> x1==y1 && x2==y2
	     | a, b when a=b              -> true
	     | _                          -> false 
	 let hash t1 =
	   match t1.reveal with
	     | Lit l        -> Atom.id l
	     | TrueP        -> 1
	     | TrueN        -> 2
	     | FalseP       -> 3
	     | FalseN       -> 4
	     | AndP (x1,x2) -> 5*x1.id+17*x2.id
	     | OrP (x1,x2)  -> 7*x1.id+19*x2.id
	     | AndN (x1,x2) -> 11*x1.id+23*x2.id
	     | OrN (x1,x2)  -> 13*x1.id+29*x2.id
       end: Hashtbl.HashedType with type t=tt)

    include MySmartFormulaImplemPrimitive

    module H = Hashtbl.Make(MySmartFormulaImplemPrimitive)

    let aset_build = function
      | Lit l        -> Some(ASet.add l ASet.empty)
      | AndP (x1,x2) -> (match x1.aset, x2.aset with
			   | Some(a),Some(b) -> Some(ASet.union a b)
			       (* | Some a,None -> Some a *)
			       (* | None,Some b -> Some b *)
			   (* | _,_   -> None) *)
			   | _,_   -> failwith("None found in aset_build"))
      | _            -> Some(ASet.empty)
      (* | _            -> None *)

    (* Constructing a formula with HConsing techniques *)

    let table = H.create 5003
    let funique =ref 0

    module FI = struct
      type t = tt
      let build a =
	let f = {reveal =  a; id = !funique; aset = aset_build a} in
	  try H.find table f
	  with Not_found -> incr funique; H.add table f f; f

      let reveal f = f.reveal
    end

    let build  = FI.build
    let reveal = FI.reveal

    let compare t1 t2 = 
      let g = Pervasives.compare t1.id t2.id in
	(*     print_endline("Comparing "^string_of_int t1.id^" "^string_of_int t2.id^" "^string_of_int g^" "^(
	       match t1.aset,t2.aset with
	       | Some a,Some b -> ASet.toString a^" "^ASet.toString b
	       | Some a,None -> ASet.toString a^" second no aset"
	       | None,Some a -> "First no aset "^ASet.toString a
	       | None,None -> "No asets"
	       )
	       );*)
	g
	  
	  
    let clear() = H.clear table
  end




  (* ******************************************* *)
  (* Implementation of sets of formulae for DPLL *)

  module FSet = struct

    module UF   = PrintableFormula(Atom)(F)
    module UT0  = TypesFromCollect(struct
				     include ASet 
				     type keys=t 
				     let tag s= s 
				   end)
    module UT1  = Lift(struct
			 include UT0 
			 type newkeys=F.t 
			 let project = F.aset
		       end)
    module UT2  = struct
      include UT1 
      let pequals = UT1.pequals UT0.pequals 
    end
    module UT3  = TypesFromHConsed(struct
				     type t = F.t 
				     let id = F.id 
				   end)

    module UT   = struct
      include LexProduct(UT2)(UT3)
      (* let match_prefix (q1,q2) (p1,p2) m =  *)
      (* 	if q2==636 || p2==636 then  *)
      (* 	  ( *)
      (* 	    print_endline("\nStarting match_prefix "^string_of_int q2^" "^string_of_int p2); *)
      (* 	    (match q1,p1 with *)
      (* 	       |(Some(h,_)),(Some(h',_)) -> print_endline("q1= "^string_of_int(ASet.AtSet.id h)^" q1= "^(ASet.AtSet.toString h)^" p1="^string_of_int(ASet.AtSet.id h')^" p1= "^(ASet.AtSet.toString h')) *)
      (* 	       | _ -> ()); *)
      (* 	    let b = match_prefix (q1,q2) (p1,p2) m in *)
      (* 	      ((\* print_endline("TT"); *\) *)
      (* 		match m with *)
      (* 		  | A(Some a)->print_endline("match_prefix up to "^Atom.toString a^" "^string_of_bool b) *)
      (* 		  | _   -> print_endline("match_prefix up to id "^string_of_bool b)); *)
      (* 	      b *)
      (* 	  ) *)
      (* 	else *)
      (* 	  match_prefix (q1,q2) (p1,p2) m *)

      let compare = F.compare
      (* let toString f = string_of_int (F.id f) *)
      let toString = UF.toString
      let cstring (a,b) = match a with
	| None -> "NC"
	| Some aa-> string_of_int (ASet.id aa)
      let bstring = function
	| A(Some at)-> Atom.toString at
	| A(None)   -> "NC"
	| _ -> "Bits"
      let tString = None
	(* Some(cstring,bstring) *)
    end

    module SS = Common.Patricia_ext.MyPat(UT)
    include SS

    (* let add f u = add f u *)
    (* if not (SS.checktree [] u) then failwith("Tree not ok before adding "^string_of_int (F.id f)); *)
    (* let u' = add f u in  *)
    (* 	if not (SS.checktree [] u') then  *)
    (* 	  (print_endline(string_of_int (F.id f)^" now printing\n" *)
    (* 			 ^toString u^"\n That was before, and now after add\n" *)
    (* 			 ^toString u'); *)
    (* 	   failwith("Tree not ok after add") *)
    (* 	  ); *)
    (* if F.id f = 636 *)
    (* then (); *)
    (* u' *)
    (*  let remove f u = 
	let u' = remove f u in 
	if F.id f = 631
	then (print_endline(string_of_int (F.id f)^"\n"
	^toString u^"\n"
	^toString u');
	failwith("hh"))
	else u'*)
    let byes j         = j
    let bempty         = None
    let bsingleton j m = Some j
    let bunion a b = match a,b with
      | None, None   -> None
      | None, Some bb-> Some bb
      | _            -> failwith("Shouldn't be a union here")

    let filter atms =function
      | A(Some a)-> not (ASet.is_in (Atom.negation a) atms)
      | _        -> true

    let yes _ _ _ = Yes() 

    let rchoose atms l =
      find_su byes bsingleton bempty bunion yes true (filter atms) (function None -> true | _ -> false) (Some(atms),-1) l

  end

end

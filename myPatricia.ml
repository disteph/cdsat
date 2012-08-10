open Formulae
open MySFormulaImplem
open Collection
open Strategy
open Sequents
open Memoisation
open Patricia

module MyPatriciaCollectImplem(M:sig
				 type t
				 val id: t->int
				 val toString: t->string
			       end) = struct
  module TFHC = TypesFromHConsed(struct
				   type keys=M.t
				   let tag f = M.id f
				   type values = unit
				   let vcompare _ _ = 0
				     (* type infos = unit *)
				     (* let info_build = empty_info_build *)
				     (* Alternative, recording min, max, cardinal: *)
				   type infos = keys m_infos
				   let info_build = m_info_build tag Pervasives.compare
				   let treeHCons = !Flags.memo
				 end) 
  module SS = PATSet(TFHC)

  module CI = struct
    type e       = SS.keys
    type t       = SS.t
    let is_empty = SS.is_empty
    let is_in    = SS.mem
    let empty    = SS.empty
    let add      = SS.add
    let union    = SS.union
    let inter    = SS.inter
    let remove   = SS.remove
      (* Alternative, if min or max is recorded:

	 let choose t = match SS.info t with 
	 | _,Some y,_ -> y 
	 | _-> failwith("No Element!")
	 end	 
      *)

    let hash = SS.hash
    let equal = SS.equal

    let next  t1 = let e1 = SS.choose t1 in (e1, remove e1 t1)
    let toString = SS.toString M.toString

  end

  include CI

end

module MyPatriciaACollectImplem = struct

  module AtSet = MyPatriciaCollectImplem(Atom)
  module TFHC = TypesFromHConsed(struct
				   type keys      = bool*Atom.Predicates.t
				   let tag (b,f)  = 2*(Atom.Predicates.id f)+(if b then 1 else 0)
				   type values    = AtSet.t
				   let vcompare s1 s2 = match AtSet.SS.first_diff (AtSet.SS.info) s1 s2 with
				     | (None,_) -> 0
				     | (_,true) -> -1
				     | (_,false)-> 1
				   type infos     = (keys*values) option
				   let info_build = (
				     None,
				     (fun x y -> Some(x,y)),
				     (fun x1 x2
					-> match x1,x2 with
					  | None,_ -> x2
					  | _,None -> x1
					  | Some(y1,v1),Some(y2,v2)->
					      if (tag y1)<(tag y2) || ((tag y1)=(tag y2)&&vcompare v1 v2<0)
					      then x1 else x2)
				   )
				   let treeHCons = !Flags.memo
				 end)
  module SS      = PATMap(TFHC)

  let lleaf(k,x) = if AtSet.is_empty x then SS.empty else SS.leaf(k,x)

  module CI = struct
    type e        = Atom.t
    type t        = SS.t
    let hash      = SS.hash
    let equal     = SS.equal
    let empty     = SS.empty
    let is_empty  = SS.is_empty
    let union     = SS.union AtSet.union
    let inter     = SS.inter AtSet.inter
    let is_in l t =
      let (b,p,tl) = Atom.reveal l in
	(SS.mem (b,p) t)
	&&(AtSet.is_in l (SS.find (b,p) t))
    let add l     =
      let (b,p,tl) = Atom.reveal l in
      let f c = function
	| None   -> AtSet.SS.singleton c
	| Some(d)-> AtSet.add c d
      in SS.add f (b,p) l
    let remove l t =
      let (b,p,tl) = Atom.reveal l in
	SS.remove_aux (fun k x -> lleaf(k,AtSet.remove l x)) (b,p) t
    let next t1 =
      let (_,y) = SS.choose t1 in
      let l = AtSet.SS.choose y in
      (l, remove l t1)
    let toString = SS.toString (fun (k,l)->AtSet.toString l)
    let filter b pred t=
      if SS.mem (b,pred) t
      then SS.leaf((b,pred),SS.find (b,pred) t)
      else empty
  end

  include CI

end

module MyPAT =
  (struct

     (* User uses the smart datastructures with hconsing and sets from
	above *)

     module UF    = MySmartFormulaImplem
     module UFSet = MyPatriciaCollectImplem(struct 
					      include UF
					      let id = UF.id
					      module PF = PrintableFormula(UF)
					      let toString = PF.toString
					    end)
     module UASet = MyPatriciaACollectImplem

     module Strategy =
       functor (FE:FrontEndType with module F=UF.FI and module FSet=UFSet.CI and module ASet=UASet.CI) -> struct
	 include FE

	 type data = unit
	 let initial_data=()

	 module ASetExt = struct
	   include UASet.CI
	   let compare  = UASet.SS.compare
	   let compareE t1 t2  =
	     let (b1,pred1,tl1) = Atom.reveal t1 in 
	     let (b2,pred2,tl2) = Atom.reveal t2 in 
	     let c = Pervasives.compare (UASet.SS.tag (b1,pred1))(UASet.SS.tag (b2,pred2)) in
	       if c=0 then Pervasives.compare (Atom.id t1) (Atom.id t2) else c
	   let min s      = match UASet.SS.info s with
	     | None       -> None
	     | Some(k,v)  -> UASet.AtSet.SS.info v 
	   let diff       = UASet.SS.diff (fun k x y -> UASet.lleaf(k,UASet.AtSet.SS.diff x y))
	   let first_diff s1 s2 = match UASet.SS.first_diff UASet.SS.info s1 s2 with
	     | (None,b)      -> (None,b)
	     | (Some(k,x),b) -> let other = if b then s2 else s1 in
		 if UASet.SS.mem k other
		 then  UASet.AtSet.SS.first_diff (UASet.AtSet.SS.info) x (UASet.SS.find k other)
		 else (UASet.AtSet.SS.info x,b)
	 end

	 module FSetExt = struct
	   include UFSet.CI
	   let compare    = UFSet.SS.compare
	   let compareE   = UF.compare
	   let min        = UFSet.SS.info
	   let diff       = UFSet.SS.diff
	   let first_diff = UFSet.SS.first_diff min
	 end

	 module Me = Memo(FSetExt)(ASetExt)

	 let rec solve = function
	   | Local ans                    ->  Me.clear();ans

	   | Fake(AskFocus(_,l,machine,_)) when UFSet.is_empty l
	       -> solve (machine(Restore None))
	   | Fake(AskFocus(_,l,machine,_)) when !Flags.memo
	       -> solve (machine(Search(Me.search4failure,accept,A(Some(Focus(UFSet.SS.choose l,accept,None))))))
	   | Fake(AskFocus(_,l,machine,_))
	     -> solve (machine (Focus(UFSet.SS.choose l, accept,None)))

	   | Fake(Notify(_,machine,_))     when !Flags.memo
	       -> solve (machine ((),
				  (fun _ -> Mem(Me.tomem,Accept)),
				  Some(Search(Me.search4success,
					      accept,
					      (if !Flags.almo
					       then F(fun _->None)
						 (* | A(a)->Cut(7,UF.build(Lit(a)),accept,accept)
						    | F(f)->Cut(7,f,accept,accept)
						 *)
					       else A(None)
					      )
					     ))
				 ))
	   | Fake(Notify(_,machine,_))    -> solve (machine ((),(fun _ -> Exit(Accept)),None))

	   | Fake(AskSide(seq,machine,_)) -> solve (machine true)
	   | Fake(Stop(b1,b2, machine))   -> solve (machine ())

       end
   end:User)

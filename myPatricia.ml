open Formulae;;
open Collection;;
open Strategy;;
open Sequents;;

open Hashtbl;;

module MyOrderedSmartFormulaImplem = struct

  type tt = {reveal: tt form;id:int;priority:int}

  let id f = f.id
 
  (* HashedType for formulae *)

  module MySmartFormulaImplemPrimitive = 
    (struct
       type t = tt
       let equal t1 t2 =
	 (* print_endline "equal"; *)
	 match t1.reveal,t2.reveal with
	   | Lit (b,x1,x2), Lit (c,y1,y2) -> b=c && x1=y1 && x2=y2
	   | AndP (x1,x2), AndP (y1,y2)   -> x1==y1 && x2==y2
	   | OrP (x1,x2), OrP (y1,y2)     -> x1==y1 && x2==y2
	   | AndN (x1,x2), AndN (y1,y2)   -> x1==y1 && x2==y2
	   | OrN (x1,x2), OrN (y1,y2)     -> x1==y1 && x2==y2
	   | _                            -> false 
       let hash t1 =
	 (* print_endline "hash"; *)
	 match t1.reveal with
	   | Lit (b,x1,x2)   -> Hashtbl.hash (b,x1,x2)
	   | AndP (x1,x2)    -> 5*x1.id+17*x2.id
	   | OrP (x1,x2)     -> 7*x1.id+19*x2.id
	   | AndN (x1,x2)    -> 11*x1.id+23*x2.id
	   | OrN (x1,x2)     -> 13*x1.id+29*x2.id
     end: Hashtbl.HashedType with type t=tt)

  include MySmartFormulaImplemPrimitive

  module H = Hashtbl.Make(MySmartFormulaImplemPrimitive)
  let reveal f = f.reveal

  (* Function computing a priority for each 
   * When picking a formula among a set, the formula with highest priority will be picked first
   * Here, smallest formulae have highest priority
   *)

  let prior = function
    | Lit (b,x1,x2)   -> - 1
    | AndP (x1,x2)    -> - x1.priority - x2.priority
    | OrP (x1,x2)     -> - x1.priority - x2.priority
    | AndN (x1,x2)    -> - x1.priority - x2.priority
    | OrN (x1,x2)     -> - x1.priority - x2.priority

  (* Constructing a formula with HConsing techniques *)

  let table = H.create 5003
  let unique =ref 0
  let build a =
    let f = {reveal =  a; id = !unique; priority = prior a} in
      try H.find table f
      with Not_found -> incr unique; H.add table f f; f

  (* Constructing a formula with HConsing techniques *)

  let compare t1 t2 =
    let a = (Pervasives.compare t1.priority t2.priority) in
      if (a<>0) then  a else (Pervasives.compare t1.id t2.id)
end
;;

open Patricia;;

module MyPatriciaCollectImplem(M:sig
				 include FormulaImplem
				 val id: t->int
			       end) = 
  (struct
     module TFHC = TypesFromHConsed(struct
				      type keys=M.t
				      let tag f = M.id f
				      type values = unit
				      let vcompare _ _ = true
				      type infos = unit
				      let info_build = ((),(fun _ _ ->()),(fun _ _-> ()))
					(* Alternative, recording min, max, cardinal:

					  type infos = (keys option)*(keys option)*unit
					  let info_build = (
					  (None,None,()),
					  (fun x _ ->(Some x,Some x,())),
					  (fun (x1,y1,z1) (x2,y2,z2)
					  -> ((match x1,x2 with
					  None,_ -> x2
					  | _,None -> x1
					  | Some(v1),Some(v2)-> if(tag v1<tag v2)then x1 else x2),
					  (match y1,y2 with
					  None,_ -> y2
					  | _,None -> y1
					  | Some(v1),Some(v2)-> if(tag v1>tag v2)then y1 else y2),
					  ()))
					  )
					*)
				    end) 
     module SS    = PATSet(TFHC)
     type e       = SS.keys
     type t       = SS.t
     let is_empty = SS.is_empty
     let is_in    = SS.mem
     let empty    = SS.empty
     let add      = SS.add
     let union    = SS.union
     let remove   = SS.remove
     let choose   = SS.choose
       (* Alternative, if min or max is recorded:

	  let choose t = let (_,x,_)=SS.info t in 
	       begin
	       match x with
	       | Some(y)->y 
	       | _-> failwith("No Element!")
	       end	 
       *)
     let next  t1 = let e1 = choose t1 in (e1, remove e1 t1)
     module PF = PrintableFormula(M)
     let toString t1 = 
       let rec toString_aux = function
	   [] -> ""
	 | f::[] -> PF.toString(f)
	 | f::l -> PF.toString(f)^", "^(toString_aux l)
       in
	 toString_aux (SS.elements t1)
   end:CollectImplem with type e = M.t)
;;

module MyPAT =
  (struct

     (* User uses the smart datastructures with hconsing and sets from
     above *)

     module UF    = MyOrderedSmartFormulaImplem
     module UFSet = MyPatriciaCollectImplem(struct 
					      include UF
					      let id = UF.id
					    end)
     module UASet = MyCollectImplem(Atom)

     (* Below are the restricted version of the above, where the
     peculiarities of the implementations are hidden before these are
     fed to the frontend *)
     
     module F: (FormulaImplem with type t = UF.t) = UF
     module FSet: (CollectImplem with type e = F.t and type t=UFSet.t) = UFSet
     module ASet: (CollectImplem with type e = Atom.t and type t=UASet.t) = UASet

     module Strategy =
       functor (FE:FrontEndType with module F=F and module FSet=FSet and module ASet=ASet) -> struct
	 include FE

	 (* As in the default implementation of user's strategies,
	    this strategy provides the following function solve:
	    In case the temporary answers happens to be a final
	    answer, then the strategy returns that final answer.
	    Otherwise, the temporary answer always contains a
	    computing machine that can be triggered by inserting a
	    "coin" - the user can orient the computation by
	    choosing which coin they insert (typically, which
	    formula to place in the next focus - here: the first
	    available one, but using the choose function of
	    UFSet) *)

	 let rec solve = function
	   | Local ans                  -> ans
	   | Fake(AskFocus(seq,machine))-> solve (machine (match seq with
							     | Seq.EntUF(_,_, l, _, _,_) -> Focus(UFSet.choose l, accept)
							     | _ -> failwith("No more formula to place focus on.")
							  ))
	   | Fake(AskSide(seq,machine)) -> solve(machine true)
	   | Fake(Stop(b1,b2, machine)) -> solve(machine ())

       end
   end:User)
;;

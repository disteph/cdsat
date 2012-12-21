open Kernel

open Formulae
open Collection
open Sequents

(* Default implementation for interface FormulaImplem *)

module MyFormulaImplem = 
  (struct
     type t = Reveal of t form

     let reveal (Reveal a) = a
     let build a = (Reveal a)
   end:FormulaImplem)

(* Default implementation for interface CollectImplem *)

module type PrintableType = sig 
  type t 
  val toString: t -> string
end

module MyCollectImplem (MyPType:PrintableType) =
  (struct
     type e = MyPType.t
     type t = e list
	 
     let is_empty = function 
	 [] -> true
       | _ -> false
     let rec is_in x = function
	 [] -> false
       | y::l when y=x -> true
       | y::l -> is_in x l
     let empty = [] 
     let add x l = x::l
     let rec union gamma1 = function
	 [] -> gamma1
       | a::gamma2 -> a::(union gamma1 gamma2)
     let rec inter gamma1 = function
	 [] -> []
       | a::gamma2 -> let gamma3 = inter gamma1 gamma2 in
	   if is_in a gamma1 then a::gamma3 else gamma3
     let rec remove x = function
	 [] -> failwith(MyPType.toString(x)^" is not in list!")
       | y::l when y=x -> l
       | y::l -> y::(remove x l)
     let next = function
	 (a::l) -> (a,l)
       | _ -> failwith("No more item to pick")
     let rec toString = function
	 [] -> ""
       | f::[] -> MyPType.toString(f)
       | f::l -> MyPType.toString(f)^", "^(toString l)
     let hash = Hashtbl.hash
     let equal = (=)
   end:CollectImplem with type e = MyPType.t and type t = MyPType.t list)

(* Default implementation for interface ACollectImplem *)

module MyACollectImplem =
  (struct
     module CI = 
       (struct
	  include MyCollectImplem(Atom)
	  let rec filter b pred = function
	    | []   -> []
	    | a::l -> let l' = filter b pred l in
	      let (b',pred',tl) = Atom.reveal a in
		if (b=b')&& (Atom.Predicates.compare pred pred' =0) then a::l' else l'
	end:ACollectImplem)
     include CI
     let hd l = let (a,_) = next l in a
     let rec length l = if is_empty l
     then 0
     else let (_,b) = next l in 1+length b
   end)


    

(* Default implementation for interface User *)

  module UF    = MyFormulaImplem
  module UFSet = MyCollectImplem(PrintableFormula(UF))
  module UASet = MyACollectImplem
  module PF = Formulae.PrintableFormula(UF)
  module Strategy (FE:FrontEndType with module F=UF and module FSet=UFSet and module ASet=UASet.CI) = struct
    include FE
      (* The strategy provides the following function solve:
	 In case the temporary answers happens to be a final
	 answer, then the strategy returns that final answer.
	 Otherwise, the temporary answer always contains a
	 computing machine that can be triggered by inserting a
	 "coin" - the user can orient the computation by
	 choosing which coin they insert (typically, which
	 formula to place in the next focus - here: the first
	 available one) *)


    module H = Hashtbl.Make(Atom)

    let table = H.create 5003
    let unique = ref 0
    let free = ref UASet.empty
      
    (*let rec toString1 l = match l with
      | [] -> ";"
      | a::x -> U.toString a^";"^toString1 x
      let toString2 x y = let (y1,y2) = y in print_endline(Atom.toString x^" : "^(match y1 with |0 -> "not assigned" |1 -> "in seq" |2 -> "not in seq")^" : "^toString1 y2)
      let toString table = H.iter toString2 table*)

    let analyse_litteral l c clause=		
      if not (UASet.is_in l !free) then (free := UASet.add l !free;free:= UASet.add (Atom.negation l) !free);
      (try let _ = H.find table (Atom.negation l) in () with Not_found -> H.add table (Atom.negation l) (0,[]));
      if c<2
      then try H.replace table l (0,let (_,x) = H.find table l in clause::x);c+1 
      with Not_found -> H.add table l (0,clause::[]);
	c+1
      else try (let _= H.find table l in ());c
      with Not_found -> H.add table l (0,[]);
	c 

    let rec analyse_clause x c clause = 
      match UF.reveal x with
	|AndP(x1,x2)  -> let i = analyse_clause x1 c clause in analyse_clause x2 i clause
	|Lit l -> analyse_litteral l c clause 

    let rec analyse x = 
      if not (UFSet.is_empty x) then let a::l = x in analyse_clause a 0 a;analyse l
	
    let cut () = UF.build(let (x,_) = UASet.next !free in Lit(x))
      



      
    type data = unit
    let initial_data=()
    let sequent = ref UASet.empty
    let stack = ref UFSet.empty
    let etape = ref 1
    let prioritaire = ref None

    let rec solve = function
      | Local ans                       -> ans
      | Fake(Notify  (_,_,machine,_))   -> solve (machine (true,(),(fun _->Exit(Accept)),None))
      | Fake(AskFocus(_,[],machine,_))  -> print_endline("restore"); solve (machine (Restore None))
      | Fake(AskSide (_,machine,_))     -> solve (machine true)
      | Fake(Stop(b1,b2, machine))      -> solve (machine ())
      | Fake(AskFocus(seq,l,machine,olda))-> if !unique=0 then (unique := !unique + 1; let _=analyse l in ());
	  print_endline("Etape : "^(string_of_int) !etape);etape:=!etape+1;
	  print_endline("liste des litteraux true : "^UASet.toString (let (x,_) = Seq.simplify seq in x));	
	  (*print_endline("list des clauses restantes : "^UFSet.toString l);*)
	  print_endline("liste des litt'eraux libres : "^UASet.toString !free);
	  (*print_endline("sequent : "^UASet.toString !sequent);*)
	  (*print_endline("stack : "^UFSet.toString !stack);*)

	  prioritaire:=None;	
	  
	  let rec backtrack s1 s2 =  if UASet.length s1 > 0
	  then if (UASet.length s1 == UASet.length s2 && not (Atom.equal (UASet.hd s1) (UASet.hd s2)))|| UASet.length s1 > UASet.length s2 
	  then( 
	    let (x1,y1) = (UASet.next s1) in
	      free:=UASet.add x1 !free;free:= UASet.add (Atom.negation x1) !free;H.replace table x1 (0,let (_,x) = H.find table x1 in x); 
	      let y = Atom.negation x1 in H.replace table y (0,let (_,x) = H.find table y in x);
		stack := UFSet.empty;sequent:= UASet.remove x1 !sequent;
		backtrack y1 s2;)
	  in backtrack !sequent (let (x,_) = Seq.simplify seq in x);
	    
	    let rec diff s1 s2 = 
	      if UASet.length s2 > UASet.length s1
	      then begin let (x2,y2) = (UASet.next s2) in
		if UASet.is_in x2 !free then free := UASet.remove x2 !free;
		if UASet.is_in (Atom.negation x2) !free then free := UASet.remove (Atom.negation x2) !free;
		H.replace table x2 (1,let (_,x) = H.find table x2 in x);
		let y = Atom.negation x2 in H.replace table y (2,let (_,x) = H.find table y in x);
		  let (_,x) = H.find table x2 in 
		  let rec clause y =
		    if not (y=[])
		    then
		      let rec anal_clause z claus free= match UF.reveal z with
			| AndP(u,v) -> let (boo,fr) = anal_clause u claus free in if boo then anal_clause v claus fr else (false,0)
			| Lit l -> let (i,_) = H.find table l in
			    match i with
			      | 2 -> (false,0)
			      | 1 -> (true,free)
			      | 0 -> if free == 1 then begin H.replace table l (0,let(_,m) = H.find table l in claus::m);
				  H.replace table x2 (1, let(_,m) = H.find table x2 in UFSet.remove claus m);
				  (false,0)
				end
				else (true,1)

				  
		      in match y with |a::b -> let (boo,fr) = anal_clause a a 0 in match boo,fr with
			|true,0 -> prioritaire := Some(a);
			|true,_ -> stack:= a::!stack; clause b
			|_,_->clause b											
		  in  (match !prioritaire with None -> let _ = clause x in ());
		    diff s1 y2
	      end
	    in diff !sequent (let (x,_) = Seq.simplify seq in x);

	      sequent := (let (x,_) = Seq.simplify seq in x);
	      (match !prioritaire with None ->
		 begin
		   let rec analyse_stack s = 
		     if not (UFSet.is_empty !s) 
		     then (let (x,y) = UFSet.next !s in s := y;
			     if UFSet.is_in x l															
			     then prioritaire:= Some(x)
			     else analyse_stack s)
		   in analyse_stack stack;
		 end
		 |_ ->  print_endline("contradiction"));
	      
	      match !prioritaire with Some(u) -> solve(machine(Focus(u,accept,None))) 
		|None -> if UASet.is_empty !free then let a::b = l in solve(machine(Focus(a,accept,None)))
		  else solve(machine(Cut(7,cut(),accept,accept,None))) 		
		    		    
  end

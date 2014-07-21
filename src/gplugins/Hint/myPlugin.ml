open Kernel

open Formulae
open Interfaces

module GenPlugin(Atom: AtomType):(Plugins.Type with type literals = Atom.t) = struct

  type literals = Atom.t


  (* Default implementation for interface FormulaImplem *)

  module MyFormulaImplem = 
    (struct
       type lit = literals
       type t = Reveal of (t,lit) form
       let reveal (Reveal a) = a
       let build a = (Reveal a)
     end : FormulaImplem with type lit = literals)

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
	 | [] -> true
	 | _ -> false
       let rec is_in x = function
	 | [] -> false
	 | y::l when y=x -> true
	 | y::l -> is_in x l
       let empty = [] 
       let add x l = if is_in x l then l else x::l
       let rec union gamma1 = function
	 | [] -> gamma1
	 | a::gamma2 when is_in a gamma1 -> union gamma1 gamma2
	 | a::gamma2 -> a::(union gamma1 gamma2)
       let rec inter gamma1 = function
	 | [] -> []
	 | a::gamma2 -> let gamma3 = inter gamma1 gamma2 in
	     if is_in a gamma1 then a::gamma3 else gamma3
       let rec remove x = function
	 | [] -> failwith(MyPType.toString(x)^" is not in list!")
	 | y::l when y=x -> l
	 | y::l -> y::(remove x l)
       let next = function
	 | (a::l) -> (a,l)
	 | [] -> failwith("No more item to pick")
       let rec fold f l0 init= match l0 with
	 | (a::l) -> fold f l (f a init)
	 | [] -> init
       let subset gamma1 gamma2 =
         fold (fun a b ->b && is_in a gamma2) gamma1 true

       let rec toString = function
	 | [] -> ""
	 | f::[] -> MyPType.toString(f)
	 | f::l -> MyPType.toString(f)^", "^(toString l)
       let hash = Hashtbl.hash
       let equal = (=)
     end: CollectImplem with type e = MyPType.t and type t = MyPType.t list)

  (* Default implementation for interface ACollectImplem *)

  module MyACollectImplem = MyCollectImplem(Atom)
(* CollectImplem with type e = literals and type t = literals list) *)

  (* Default implementation for interface User *)

  module UF    = MyFormulaImplem
  module UFSet = MyCollectImplem(PrintableFormula(Atom)(UF))
  module UASet = MyACollectImplem
  module Strategy(FE:FrontEndType with type litType     = literals
				  and  type formulaType = UF.t
				  and  type fsetType    = UFSet.t
				  and  type asetType    = UASet.t)
    = struct
      include FE
      include Common.Utils.FEext(FE)
	(* The strategy provides the following function solve:
	   In case the temporary answers happens to be a final
	   answer, then the strategy returns that final answer.
	   Otherwise, the temporary answer always contains a
	   computing machine that can be triggered by inserting a
	   "coin" - the user can orient the computation by
	   choosing which coin they insert (typically, which
	   formula to place in the next focus - here: the first
	   available one) *)

      type data = unit
      let initial_data _ = ()

      let wait () = ignore (read_line ())

      let display_aset atoms =
          let latoms = UASet.fold (fun x l -> x::l) atoms [] in
          let vatoms = Array.of_list latoms in
          let showith i a = Printf.printf "\t%d: %s\n" i (Atom.toString a) in
            Array.iteri showith vatoms

      let display_farray forms = 
          let showith i a = Printf.printf "\t%d: %s\n" i (Form.toString a) in
            Array.iteri showith forms

      let display_fset forms = 
          let lforms = UFSet.fold (fun x l -> x::l) forms [] in
          let vforms = Array.of_list lforms in
            display_farray vforms

      let print_hrule c = print_endline (String.make 79 c)

      let display_seq seq =
        let display_gen atomN formP formPSaved =
            print_endline "atomN : ";
            display_aset atomN;
            print_endline "formP:";
            display_fset formP;
            print_endline "formPSaved :";
            display_fset formPSaved
        in
            match seq with
            | Seq.EntF(atomN, g, formP, formPSaved, polar, ar) -> 
                display_gen atomN formP formPSaved;
                print_string "Goal:\t";
                print_endline ("[ " ^ (Form.toString g) ^ "]")
            | Seq.EntUF(atomN, delta, formP, formPSaved, polar, ar) ->
                display_gen atomN formP formPSaved;
                print_endline "delta:";
                display_fset delta

      let parse_abort = function 
        | "abort" | "Abort" -> raise (Plugins.PluginAbort "I abort")
        | _ -> ()

      let rec ask_side () = 
            print_string "Choose a side (left or right) > "; 
            match read_line () with
                | "left" | "Left" -> true
                | "right" | "Right" -> false
                | s -> parse_abort s; ask_side ()
                
      let re_space = Str.regexp " +"

      let rec ask_focus seq pforms more checked =
          let too_few_arguments () = 
            print_endline "ERROR: too few arguments";
            ask_focus seq pforms more checked in
          let cannot_parse_argument () = 
            print_endline "ERROR: cannot parse arguments";
            ask_focus seq pforms more checked in
          let unknown_command () = 
            print_endline "ERROR: unknown command";
            ask_focus seq pforms more checked in
          let interp_cmd cmd args = 
            let get b =
                match args with
                | [] -> Get(true, b, fNone)
                | "admit"::_ -> Get(true,b,fNone)
                | "fail"::_ -> Get(false,b,fNone)
                | _ -> cannot_parse_argument ()
            in
              match cmd with
                | "focus" | "Focus" -> 
                    let lforms = UFSet.fold (fun x l -> x::l) pforms [] in
                    let vforms = Array.of_list lforms in
                        display_farray vforms;
                        print_string "Enter a number > ";
                        (try
                            let n = read_int () in 
                            let f = Array.get vforms n in
                                Focus(f, accept, fNone)
                        with
                            | Invalid_argument msg | Failure msg ->
                                print_endline ("ERROR: " ^ msg);
                                ask_focus seq pforms more checked)
                | "check" | "Check" ->
                    if checked then
                        begin
                            print_endline "ERROR: cannot run consistency check";
                            ask_focus seq pforms more checked
                        end
                    else
                        ConsistencyCheck(accept, fNone)
                | "cut" | "Cut" -> 
                    (match args with
                        | [] -> too_few_arguments ()
                        | n::f ->
                            (* the first argument is the type of cut*)
                            (* parse the second argument as a formula *)
                            failwith "Not yet implemented")
                | "next" | "Next" ->
                    get true
                | "prev" | "previous" | "Prev" | "Previous" ->
                    get false
                | "restore" | "Restore" ->
                    if more then
                        Restore fNone
                    else
                        begin
                            print_endline "ERROR: no more formulae";
                            ask_focus seq pforms more checked
                        end
                | "propose" | "Propose" ->
                    (match args with
                      | [] -> too_few_arguments ()
                      | ans::_ ->
                        (* Parse the argument as a formula *)
                        failwith "Not yet implemented")
                | "polarize" | "Polarize" -> 
                    failwith "Not yet implemented "
                | "depolarize" | "Depolarize" | "DePolarize" -> 
                    failwith "Not yet implemented "
                | s -> parse_abort s; unknown_command ()
          in
            print_string "Enter a focus action > ";
            match Str.split re_space (read_line ()) with
              | [] -> print_endline "ERROR: empty line"; ask_focus seq pforms more checked
              | cmd::args -> interp_cmd cmd args

    let rec solve = function
      | Local ans -> ans
      | Fake(Notify  (seq,_,_,execute,_)) -> 
        print_hrule '=';
        display_seq seq;
        print_hrule '-';
        print_endline "Status: Notify";
        print_string "Hit enter to continue > ";
        wait ();
        solve (execute (true,(),accept,fNone))
      | Fake(AskFocus(seq,_,pforms,more,checked,execute,label)) -> 
        print_hrule '=';
        display_seq seq;
        print_hrule '-';
        print_endline "Status: AskFocus";
        let ans = ask_focus seq pforms more checked in
        solve (execute ans)
      | Fake(AskSide (seq, _, execute,_)) -> 
        print_hrule '=';
        display_seq seq;
        print_hrule '-';
        print_endline "Status: AskSide";
        let side = ask_side () in
        solve (execute side)
      | Fake(Stop(b1,b2, execute)) ->
        print_hrule '=';
        print_endline ("Status: No more "^(if b1 then "Success" else "Failure")^" branch on the "^(if b2 then "right" else "left"));
        solve (execute ())
	    
    end

end

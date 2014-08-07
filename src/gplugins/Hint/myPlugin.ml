open Format

open Kernel

open Interfaces_I
open Formulae
open Interfaces_II

module GenPlugin(IAtom: IAtomType)
  :(Plugins.Type with type iliterals = IAtom.t
                 and  type literals  = IAtom.Atom.t
                 and  type delsubsts = IAtom.DSubst.t) = struct
  
  type iliterals = IAtom.t
  type literals  = IAtom.Atom.t
  type delsubsts = IAtom.DSubst.t
    
  (* Default implementation for interface CollectImplem *)

  module type PrintableType = sig 
    type t 
    val print_in_fmt: formatter -> t -> unit
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
	 | [] -> failwith(Dump.toString (fun p->p "%a is not in list!" MyPType.print_in_fmt x))
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

       let rec print_in_fmt fmt = function
	 | []    -> ()
	 | f::[] -> fprintf fmt "%a" MyPType.print_in_fmt f
	 | f::l  -> fprintf fmt "%a, %a" MyPType.print_in_fmt f print_in_fmt l

     end: CollectImplem with type e = MyPType.t and type t = MyPType.t list)

  module UASet = MyCollectImplem(IAtom)
  module UF = struct
    type lit = literals
    type t = unit
    let fdata_build f = ()
  end
  module UFSet = MyCollectImplem(struct
    type t = (UF.t,UF.lit) GForm.t * IAtom.DSubst.t
    let print_in_fmt = GForm.iprint_in_fmt IAtom.Atom.print_in_fmt IAtom.DSubst.print_in_fmt
  end)

  module Strategy(FE:FrontEndType with type Form.lit    = literals
				  and  type Form.datatype = UF.t
				  and  type fsetType    = UFSet.t
				  and  type asetType    = UASet.t
				  and  type ilit        = iliterals
				  and  type dsubsts     = delsubsts) = struct

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
          let showith i a = Format.printf "\t%d: %a\n" i IAtom.print_in_fmt a in
            Array.iteri showith vatoms

      let display_farray forms = 
          let showith i a = Format.printf "\t%d: %a\n" i IForm.print_in_fmt a in
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
                Format.printf "[ %a]\n" IForm.print_in_fmt g
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

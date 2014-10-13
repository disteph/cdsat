open Format

open Kernel

open Interfaces_I
open Formulae
open Interfaces_II
open Common.Addressing

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

      type data = unit addressing
      let initial_data _ = ad_init ()

      let wait () = Format.printf "%!";ignore (read_line ())

      let display_aset atoms =
          let latoms = UASet.fold (fun x l -> x::l) atoms [] in
          let show a = Format.printf "\t%a\n%!" IAtom.print_in_fmt a in
            List.iter show latoms

      let display_farray forms = 
          let showith i a = Format.printf "\t%d: %a\n%!" i IForm.print_in_fmt a in
            Array.iteri showith forms

      let display_fset forms = 
          let lforms = UFSet.fold (fun x l -> x::l) forms [] in
          let show a = Format.printf "\t%a\n%!" IForm.print_in_fmt a in
            List.iter show lforms

      let display_ad ad =
        Format.printf "Address: %a\n%!" print_in_fmt_ad (ad[])

      let print_hrule c = print_endline (String.make 79 c)

      let display_seq seq sigma =
        let display_gen atomN formP formPSaved =
            print_endline "Atoms: ";
            display_aset atomN;
            print_endline "Positive formulae:";
            display_fset formP;
            print_endline "Positive formulae on which focus has already been placed:";
            display_fset formPSaved;
        in
        let display_full seq = 
            match seq with
            | Seq.EntF(atomN, g, formP, formPSaved, polar, ar) -> 
                display_gen atomN formP formPSaved;
                Format.printf "Goal:\t[ %a]\n%!" IForm.print_in_fmt g
            | Seq.EntUF(atomN, delta, formP, formPSaved, polar, ar) ->
                display_gen atomN formP formPSaved
        in
        display_full seq;
        print_endline "Current constraint:";
        Format.printf "%a\n%!" print_in_fmtC sigma

      let parse_abort = function 
        | "abort" | "Abort" -> raise (Plugins.PluginAbort "I abort")
        | _ -> ()

      let rec ask_side () = 
        Format.printf "Choose a side (left or right) > %!"; 
        match read_line () with
        | "left" | "Left" -> true
        | "right" | "Right" -> false
        | s -> parse_abort s; ask_side ()
                
      let re_space = Str.regexp " +"

      let rec ask_focus seq pforms more checked ad =
          let too_few_arguments () = 
            print_endline "ERROR: too few arguments";
            ask_focus seq pforms more checked ad in
          let cannot_parse_argument () = 
            print_endline "ERROR: cannot parse arguments";
            ask_focus seq pforms more checked ad in
          let unknown_command () = 
            print_endline "ERROR: unknown command";
            ask_focus seq pforms more checked ad in
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
                        (match lforms with
                            [] ->
                                print_endline "ERROR: no more positive formula to focus on";
                                ask_focus seq pforms more checked ad
                            | [f] ->
                                Focus(f, branch_one (ad[]), accept, fNone)
                            | _ ->
                                let vforms = Array.of_list lforms in
                                    display_farray vforms;
                                    Format.printf "Enter a number > %!";
                                    (try
                                        let n = read_int () in
                                        let f = Array.get vforms n in
                                            Focus(f, branch_one (ad[]), accept, fNone)
                                    with
                                        | Invalid_argument msg | Failure msg ->
                                            print_endline ("ERROR: " ^ msg);
                                            ask_focus seq pforms more checked ad)
                        )
                | "check" | "Check" ->
                    if checked then
                        begin
                            print_endline "ERROR: cannot run consistency check";
                            ask_focus seq pforms more checked ad
                        end
                    else
                        ConsistencyCheck(ad,accept, fNone)
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
                        Restore(ad,accept,fNone)
                    else
                        begin
                            print_endline "ERROR: no more formulae";
                            ask_focus seq pforms more checked ad
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
            Format.printf "Enter a focus action > %!";
            match Str.split re_space (read_line ()) with
              | [] -> print_endline "ERROR: empty line"; ask_focus seq pforms more checked ad
              | cmd::args -> interp_cmd cmd args

    let rec solve = function
      | Jackpot ans -> ans
      | InsertCoin(Notify(seq,sigma,_,execute,ad)) -> 
        print_hrule '=';
        display_ad ad;
        display_seq seq sigma;
        print_hrule '-';
        print_endline "Status: Notify";
        Format.printf "Hit enter to continue > %!";
        wait ();
        let newad = el_wrap (branch OrNode (ad [])) in
        solve (execute (true,newad,accept,fNone))
      | InsertCoin(AskFocus(seq,sigma,pforms,more,checked,execute,ad)) -> 
        print_hrule '=';
        display_ad ad;
        display_seq seq sigma;
        print_hrule '-';
        print_endline "Status: AskFocus";
        let ans = ask_focus seq pforms more checked ad in
        solve (execute ans)
      | InsertCoin(AskSide (seq,sigma, execute,ad)) -> 
        print_hrule '=';
        display_ad ad;
        display_seq seq sigma;
        print_hrule '-';
        print_endline "Status: AskSide";
        let side = ask_side () in
        solve (execute(side,branch_two(branch OrNode (ad[]))))
      | InsertCoin(Stop(b1,b2, execute)) ->
        print_hrule '=';
        print_endline ("Status: No more "^(if b1 then "Success" else "Failure")^" branch on the "^(if b2 then "right" else "left"));
        solve (execute ())
	    
    end

end

open Format

open Kernel
open Prop

open Interfaces_theory
open Literals
open Formulae
open Interfaces_plugin
open PluginsG_tools.Addressing

module DS = PluginsG_tools.ListDS.Generate

module Strategy(FE:FrontEndType with type IForm.datatype = DS.UF.t
				and  type FSet.ps     = DS.UFSet.t
				and  type ASet.ps     = DS.UASet.t) = struct

  open DS
  open FE
  include PluginsG_tools.Utils.FEext(FE)
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
    let show a = Format.printf "\t%a\n%!" LitF.print_in_fmt a in
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
        display_gen (ASet.forPlugin atomN) (FSet.forPlugin formP) (FSet.forPlugin formPSaved);
        Format.printf "Goal:\t[ %a]\n%!" IForm.print_in_fmt g
      | Seq.EntUF(atomN, delta, formP, formPSaved, polar, ar) ->
        display_gen (ASet.forPlugin atomN) (FSet.forPlugin formP) (FSet.forPlugin formPSaved)
    in
    display_full seq;
    print_endline "Current constraint:";
    Format.printf "%a\n%!" print_in_fmtC sigma

  let parse_abort = function 
    | "abort" | "Abort" -> raise (PluginG.PluginAbort "I abort")
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
      let ans = ask_focus seq (FSet.forPlugin pforms) more checked ad in
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

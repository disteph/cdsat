(*******************************)
(* Main entry point for Kernel *)
(*******************************)

open General
open Sums
open Top
open Messages
open Sassigns

let init
      ?(withtheories=Some[]) (* List of added theories *)
      ?(withouttheories=Some[]) (* List of forbidden theories *)
      ~parser
      input
  =

  let th,termB,expected = Parsers.Register.parse parser input in
  (* Now we look at the theories involved *)
  let open Theories.Register in
  let theories =
    match withtheories, withouttheories, th with
    | Some l,Some l', Some parsed ->
       HandlersMap.diff (HandlersMap.union (get parsed) (get l)) (get l')
    | None,Some l',_
      | _,Some l',None   -> HandlersMap.diff all_theories (get l')
    | None,None,None     -> all_theories
    | Some l, None,_  
      | None,None,Some l -> get l
  in
  print_endline(Print.toString (fun p->
                    p "Using theories: %a" HandlersMap.pp theories));

  (* Now that we know the theories, we build the combined datastructures *)
  let (module C) = Combo.make theories in
  (module struct
     include C
     open WB
     let problem = List.fold
                     (fun formula
                      -> DS.Assign.add (boolassign(DS.Term.lift [] formula)))
                     termB
                     DS.Assign.empty

     let expected = expected

     type 'proof answer =
       | UNSAT of (unsat,'proof) WB.t
       | SAT of WB.DS.Assign.t
       | NotAnsweringProblem

     let answer =
       let open DS in
       function
       | Case1(WB.WB(_,Propa(assign,Unsat),_) as msg) ->
          if Assign.subset assign problem
          then UNSAT msg
          else
            (print_endline(
                 Print.toString (fun p->
                     p "You said %a but this involves new hypotheses %a"
                       pp msg
                       Assign.pp (Assign.diff assign problem)));
             NotAnsweringProblem)    
                    
       | Case2(WB.Done(assign,sharing)) ->
          if Assign.subset problem assign then SAT assign
          else
            (print_endline(
                 Print.toString (fun p->
                     p "You said %a but this ignores hypotheses %a"
                       Assign.pp assign
                       Assign.pp (Assign.diff problem assign)));
             NotAnsweringProblem)

       | Case2 _ ->
          print_endline(Print.toString (fun p->
                            p "Errr... not all theories agree on model"));
          NotAnsweringProblem

   end : Export.APIext)

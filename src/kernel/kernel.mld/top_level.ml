(*******************************)
(* Main entry point for Kernel *)
(*******************************)

open General.Sums
open Top
open Messages

let init (type uaset)(type uf)(type ufset)
      (plDS : (module Theories.Prop.APIplugin.PlugDSType with type UASet.t= uaset
                                                          and type UF.t   = uf
                                                          and type UFSet.t= ufset))
      ?(disableProp=false)
      ?(withtheories=Some[]) (* List of added theories *)
      ?(withouttheories=Some[]) (* List of forbidden theories *)
      ~parser
      input
  =

  let th,termB,expected = Parsers.Register.parse parser ~disableProp input in
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
  print_endline(Dump.toString (fun p->
                    p "Using theories: %a" HandlersMap.pp theories));

  (* Now that we know the theories, we build the combined datastructures *)
  let (module C) = Combo.make theories plDS in
  (module struct
     include C
     open WB
     let problem = List.fold
                     (fun formula
                      -> DS.Assign.add (Values.bassign(DS.Term.lift [] formula)))
                     termB
                     DS.Assign.empty

     let expected = expected

     type answer =
       | UNSAT of unsat WB.t
       | SAT of sat WB.t
       | NotAnsweringProblem

     let answer = function
       | Case1(WB.WB(_,Propa(assign,Unsat)) as msg)
            when DS.Assign.subset assign problem
         -> UNSAT(msg)
       | Case2(WB.WB(_,Sat assign) as msg)
            when DS.Assign.subset problem assign -> SAT(msg)
       | _ -> NotAnsweringProblem

   end : Export.APIext with type uaset = uaset
                        and type uf    = uf
                        and type ufset = ufset)

(*********************)
(* Conflict analysis *)
(*********************)

open Async
       
open Kernel
open Combo
open Top
open Messages

open General
open Patricia_interfaces
open Patricia
open SetConstructions
open Sums
       
(* This module implements conflict analysis *)

module Make(WB : WhiteBoard) = struct

  open WB.DS
         
  module I = TypesFromHConsed(struct include Term let id = Terms.id end)

  (* type nature indicates the status of each formula accumulated in the trail so far.
The reason it was added to it was either:
- It was in the original problem
- It was propagated from other formulae already present in the trail
- It was a decision
- It was tried (i.e. a decision that is not on a formula of the finite basis, and therefore can't just be switched when realising this decision leads to conflict)
   *)
                             
  type nature =
    | Original
    | Propagated of straight WB.t
    | Decided of both WB.t
    | Tried


  (* When doing conflict analysis, the current set of formulae in conflict is implemented in type t below. In that set we want to retain some general information:
- level: the maximal level (or possibly an upper bound on it)
- chrono: the timestamp of the latest formula in the conflict
- term: that formula
- nature: the nature of this formula, as above
- is_uip: is true iff this formula is a UIP
- has_guess: is true iff the highest level is that of a guess (formula whose nature is Tried)
 *)

  type max = {
      level : int;
      chrono : int;
      term : unit -> Term.t;
      nature : unit -> nature;
      is_uip : unit -> bool;
      has_guess : unit -> bool
    }

  module DestWInfo = struct
    type keys    = Term.t
    let kcompare = Terms.compare
    type values  = int*int*nature
    type infos   = max

    let info_build = {

        empty_info  = {
          level = -1;
          chrono = -1;
          term =  (fun()->failwith "Empty Trail");
          nature = (fun()->failwith "Empty Trail");
          is_uip = (fun()->failwith "Empty Trail");
          has_guess = (fun()->failwith "Empty Trail");
        };

        leaf_info = (fun x (i,j,v) ->
          {
            level = i;
            chrono = j;
            term = (fun()-> x);
            nature = (fun()-> v);
            is_uip = (fun()-> true);
            has_guess =
              match v with
              | Tried -> fun()->true
              | Decided _ | Propagated _ | Original -> fun()->false;
          }
        );

        branch_info = (fun a b ->
          if a.level < b.level then b
          else if a.level > b.level then a
          else
            let c =
              if a.chrono < b.chrono then b else a
            in
            { c with
              is_uip    = (fun() -> false);
              has_guess = (fun() -> a.has_guess() || b.has_guess()) }
        );
      }
                       
    let treeHCons  = None (* Some(LitF.id,Terms.id,Terms.equal) *)

  end

  (* Creating the data-structure for Trails *)
                       
  include PATMap.Make(DestWInfo)(I)

  (* Extracts from trail the fragment tset that is used in left-hand
  side of propa message, from which we removed semsplit. I.e. the
  formulae in semsplit do not end up in the extraction. *)
                     
  let get_data trail ?(semsplit=TSet.empty) msg =
    let WB.WB(_,Propa(tset,_)) = msg in
    inter_poly (fun _ v () ->v) trail (TSet.diff tset semsplit)

               
  let analyse trail conflict learn =
    
    let rec aux
              conflict (* The conflict to analyse *)
              ?level   (* Level of latest contributing decision, only present if it is a guess *)
              semsplit (* This is where we collect semsplit formulae *)
              data     (* Data about { conflict formulae minus semsplit formulae } *)
            : (unsat WB.t, int * straight WB.t) sum Deferred.t
      =

      Dump.print ["trail",1] (fun p ->
          p "Analysing Conflict: %a" WB.print_in_fmt conflict);

      (* This is what we do when we finalise an answer:
       msg is the propagation message we use for the branch we backjump to.
       *)
      
      let finalise msg =
        (* First computing the formulae we need for the branch we backjump to *)
        let next = get_data trail msg in
        (* Second formula participating to conflict *)
        let second = if is_empty next then None
                     else Some((info next).term()) in
        Dump.print ["trail",1] (fun p ->
            p "Conflict level: %i; first term: %a; second level: %i; second term:%a, inferred propagation:\n%a"
              data.level
              Term.print_in_fmt (data.term())
              (info next).level
              (pp_print_option Term.print_in_fmt) second
              WB.print_in_fmt msg
          );
        (* We learn the conflict, watching first and second formulae *)

        learn conflict (data.term()) second >>| fun ()->
        (* Now jumping to level of second formula contributing to conflict *)
        
        Case2((info next).level, msg)
      in

      match level with
      | Some level when level > data.level ->
         (* Presence of level means latest contributing decision, of
          level level, is a guess.  Everything in the conflict that is
          of level >= level is a semsplit element.  It's time to do
          the split.  We want to learn the conflict as a clause, so we
          pick 2 formulae to watch. *)
         
         let t1,rest = TSet.next semsplit in
         let t2,_ = TSet.next rest in
         learn conflict t1 (Some t2) >>| fun ()->
         (* Now we output the level to backtrack to, and by curryfying the
          semsplit formulae we create a propagation message for second branch *)

         Case2(level-1, WB.curryfy semsplit conflict)

      | _ -> (* Otherwise we analyse the nature of the latest formula
              contributing to the conflict *)

         begin match data.nature() with

         | Tried -> failwith "Latest formula in conflict is a guess: TO BE IMPLEMENTED"
                             
         | Original ->
            (* It's a formula of the original problem, and so is the
               whole conflict.  We stop. *)

            return(Case1 conflict)

         | Decided msg  ->
            (* It's a decision created by PropaBoth message msg.  We
               create the propagation message we use for the backjump
               branch, and finalise our answer. *)

            finalise (WB.both2straight msg conflict)

         | Propagated msg ->

            let curry =
              if (TSet.is_empty semsplit) (* If we are not in semsplit *)
                 && (data.is_uip())       (* and data.term() is a UIP *)
                 && data.level > 0        (* and conflict is not of level 0 *)
              then (* ...we might use that UIP to switch branches *)
                Some((WB.curryfy (TSet.singleton(data.term())) conflict))
              else None
            in
            begin match curry with
            | Some(WB.WB(_,Propa(_,Straight tset)) as msg)
                 when not(subset_poly (fun() _ -> true) tset trail)
                         (* ...but only if the negation of the UIP is new knowledge *)
              -> finalise msg
            | _ ->
               Dump.print ["trail",1] (fun p ->
                   let level,chrono,_ = find (data.term()) trail in
                   p "Explaining %a of level %i and chrono %i, using message\n%a"
                     Term.print_in_fmt (data.term())
                     level chrono
                     WB.print_in_fmt msg
                 );
               (* Proposed new conflict, and its data *)
               let newconflict = WB.resolve msg conflict in
               let newdata = info(get_data trail ~semsplit newconflict) in
               if newdata.has_guess()
               then
                 (* The latest contributing decision is a guess,
                 and is among the nodes we are about to add:
                 we refuse to resolve *)
                 aux conflict ~level:data.level (TSet.add (data.term()) semsplit) newdata
               else
                 aux newconflict ?level semsplit newdata
            end
         end
    in aux conflict TSet.empty (info(get_data trail conflict))
      
end

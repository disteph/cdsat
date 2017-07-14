(*********************)
(* Conflict analysis *)
(*********************)

open Async
       
open Kernel
open Top
open Messages

open General
open Patricia_interfaces
open Patricia
open SetConstructions
open Sums

(* This module implements conflict analysis *)

module Make(WB : Export.WhiteBoard) = struct

  open WB.DS
  type sassign = Term.t*(Value.t Values.t) [@@deriving show]
         
  module I = TypesFromHConsed(SAssign)

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
    | Tried


  (* When doing conflict analysis, the current set of assignments in conflict is implemented in type t below. In that set we want to retain some general information:
- level: the maximal level (or possibly an upper bound on it)
- chrono: the timestamp of the latest assignment in the conflict
- sassign: that assignment
- nature: the nature of this formula, as above
- is_uip: is true iff this formula is a UIP
- has_guess: is true iff the highest level is that of a guess (formula whose nature is Tried)
 *)

  type max = {
      level : int;
      chrono : int;
      sassign: unit -> sassign;
      nature : unit -> nature;
      is_uip : unit -> bool;
      has_guess : unit -> bool
    }

  module DestWInfo = struct
    type keys    = SAssign.t
    let kcompare = SAssign.compare
    type values  = int*int*nature
    type infos   = max

    let info_build = {

        empty_info  = {
          level = -1;
          chrono = -1;
          sassign=  (fun()->failwith "Empty Trail");
          nature = (fun()->failwith "Empty Trail");
          is_uip = (fun()->failwith "Empty Trail");
          has_guess = (fun()->failwith "Empty Trail");
        };

        leaf_info = (fun x (i,j,v) ->
          {
            level = i;
            chrono = j;
            sassign= (fun()-> SAssign.reveal x);
            nature = (fun()-> v);
            is_uip = (fun()-> true);
            has_guess =
              match v with
              | Tried -> fun()->true
              | Propagated _ | Original -> fun()->false;
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

  (* Extracts from trail the fragment that is used in left-hand
  side of propa message, from which we removed semsplit. I.e. the
  formulae in semsplit do not end up in the extraction. *)
                     
  let get_data trail ?(semsplit=Assign.empty) msg =
    let WB.WB(_,Propa(tset,_)) = msg in
    inter_poly (fun _ v () ->v) trail (Assign.diff tset semsplit)

               
  let analyse trail conflict learn =
    
    let rec aux
              conflict (* The conflict to analyse *)
              ?level   (* Level of latest contributing decision, only present if it is a guess *)
              semsplit (* This is where we collect semsplit formulae *)
              data     (* Data about { conflict formulae minus semsplit formulae } *)
            : (unsat WB.t, int * straight WB.t) sum Deferred.t
      =

      Dump.print ["trail",1] (fun p ->
          p "Analysing Conflict: %a" WB.pp conflict);

      (* This is what we do when we finalise an answer:
       msg is the propagation message we use for the branch we backjump to.
       *)
      
      let finalise msg =
        (* First computing the assignments we need for the branch we backjump to *)
        let next = get_data trail msg in
        (* Second assignment participating to conflict *)
        let second = if is_empty next then None
                     else Some((info next).sassign()) in
        Dump.print ["trail",1] (fun p ->
            p "Conflict level: %i; first term: %a; second level: %i; second term:%a, inferred propagation:\n%a"
              data.level
              pp_sassign (data.sassign())
              (info next).level
              (Opt.pp_print_option SAssign.pp) (Opt.map SAssign.build second)
              WB.pp msg
          );
        (* We learn the conflict, watching first and second formulae *)

        learn conflict (data.sassign()) second >>| fun ()->
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
         
         let t1,rest = Assign.next semsplit in
         let t2,_ = Assign.next rest in
         learn conflict t1 (Some t2) >>| fun ()->
         (* Now we output the level to backtrack to, and by curryfying the
          semsplit formulae we create a propagation message for second branch *)

         Case2(level-1, WB.curryfy ~assign:semsplit conflict)

      | _ -> (* Otherwise we analyse the nature of the latest assignment
              contributing to the conflict *)

         begin match data.nature(),data.sassign() with

         | Original,_ ->
            (* It's a formula of the original problem, and so is the
               whole conflict.  We stop. *)

            return(Case1 conflict)

         | _,(t,Values.Boolean b)
              when (Assign.is_empty semsplit) (* If we are not in semsplit *)
                   && (data.is_uip())       (* and data.term() is a UIP *)
                   && data.level > 0        (* and conflict is not of level 0 *)
                   && not(mem (SAssign.build(Values.bassign ~b:(not b) t)) trail)
                         (* ...if the negation of the UIP is new knowledge *)
           -> (* ...we might use that UIP to switch branches *)
            finalise (WB.curryfy ~flip:(t,b) conflict)
                     
         | Propagated msg,_ ->
            begin
              Dump.print ["trail",1] (fun p ->
                  let level,chrono,_ = find (SAssign.build(data.sassign())) trail in
                  p "Explaining %a of level %i and chrono %i, using message\n%a"
                    pp_sassign (data.sassign())
                    level chrono
                    WB.pp msg
                );
              (* Proposed new conflict, and its data *)
              let newconflict = WB.resolve msg conflict in
              let newdata = info(get_data trail ~semsplit newconflict) in
              if newdata.has_guess()
              then
                (* The latest contributing decision is a guess,
                 and is among the nodes we are about to add:
                 we refuse to resolve *)
                aux conflict ~level:data.level
                  (Assign.add (data.sassign()) semsplit) newdata
              else
                aux newconflict ?level semsplit newdata
            end

         | Tried,_ -> failwith "There is a guess in the original conflict!!! Not supported (should apply rule Undo)"
                             
         end
    in aux conflict Assign.empty (info(get_data trail conflict))
           
end

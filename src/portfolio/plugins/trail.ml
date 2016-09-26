(*********************)
(* Conflict analysis *)
(*********************)

open Async.Std
       
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

The bool in propagated indicates whether we know the level of this propagation for certain, i.e. we have exhibited a dependency path from the decision of that level.
   *)
                             
  type nature =
    | Propagated of straight WB.t
    | Decided of both WB.t
    | Tried
    | Original


  (* When doing conflict analysis, the current set of formulae in conflict is implemented as a Conflict.t below. In that set we want to retain some general information:
- level: the maximal level (or possibly an upper bound on it)
- chrono: the timestamp of the latest formula in the conflict
- term: that formula
- nature: the nature of this formula, as above
- is_uip: is (Some true) if this formula is a UIP, (Some false) if it is definitely not, None if we can't tell - because some other formulae declared of the same level have not confirmed that level *)
                             
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
          term =  (fun()->failwith "Empty TTrack");
          nature = (fun()->failwith "Empty TTrack");
          is_uip = (fun()->failwith "Empty TTrack");
          has_guess = (fun()->failwith "Empty TTrack");
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

  include PATMap.Make(DestWInfo)(I)

  let get_data trail ?(semsplit=TSet.empty) msg =
    let WB.WB(_,Propa(tset,_)) = msg in
    inter_poly (fun _ () v->v) (TSet.diff tset semsplit) trail

  let rec analyse
            (trail : t)
            ?(semsplit=TSet.empty)  (* This is where we collect semsplit points *)
            ?level                  (* Level of latest contributing decision, only present if it is a guess *)
            (conflict : unsat WB.t) (* The conflict to analyse *)
            ?(conflictWdata = get_data trail ~semsplit conflict) (* The conflict's data *)
            learn                   (* A function to which we can pass stuff to learn *)
          : (unsat WB.t, int * straight WB.t) sum Deferred.t
    =

    Dump.print ["trail",1] (fun p ->
        p "Analysing Conflict: %a" WB.print_in_fmt conflict);

    let data = info conflictWdata in

    let finalise msg =
      let next = get_data trail msg in          
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
      learn conflict (data.term()) second >>| fun ()->
      Case2((info next).level, msg)
    in

    match level with
    | Some level when level > data.level
      -> (* Presence of level means latest contributing decision, of level level, is a guess.
            Everything in the conflict that is of level >= level is a semsplit element.
            It's time to do the split.
          *)
       let t1,rest = TSet.next semsplit in
       let t2,_ = TSet.next rest in
       learn conflict t1 (Some t2) >>| fun ()->
       Case2(level-1, WB.curryfy semsplit conflict)

    | _ ->
       begin match data.nature() with

       | Tried -> failwith "Latest formula in conflict is a guess"    
                           
       | Original     -> return(Case1 conflict)

       | Decided msg  -> finalise (WB.both2straight msg conflict)

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
             let newconflictWdata = get_data trail ~semsplit newconflict  in
             if (info newconflictWdata).has_guess()
             then
               (* The latest contributing decision is a guess,
                 and is among the nodes we are about to add:
                 we refuse to resolve *)
               analyse trail
                 ~semsplit:(TSet.add (data.term()) semsplit)
                 ~level:data.level
                 conflict
                 ~conflictWdata
                 learn
             else
               analyse trail
                 ~semsplit
                 ?level
                 newconflict
                 ~conflictWdata:newconflictWdata
                 learn
          end
       end
      
end

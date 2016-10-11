open Top
open Messages
open Specs

open Prop.Literals

open General
open Patricia_interfaces

open MyStructures
       
type sign = unit
  
module Make(DS: sig 
                include GTheoryDSType
                val proj: Term.datatype -> ThDS.t
              end) = struct

  open DS
         
  include Explanations.Make(DS)

  (* Type of the data-structures recording which literals are fixed:
  - field asTrueFalse
fixed literals, mapped to the "literal terms" whose assertion made
these literals determined.
   - fields justification & propagation: 
the former maps each propagated "literal term" to the unit clause that
propagated it; the latter is a queue that contains the same elements
as the domain of the justification map (but ordered chronologically).
These two fields work together, and only temporarily keep these
"literal terms" until the propagation messages are formed and sent.
   - field clauses:
clauses that have not been satisfied yet
   - field seen:
terms seen so far
  *)

  type fixed = {
      asTrueFalse  : Model.t;
      justification: uc_clause T2Clause.t;
      propagation  : Term.t Pqueue.t;
      unfolds      : straight Pqueue.t;
      clauses         : TSet.t;
      seen         : TSet.t
    }

  (* init_fixed is the initial structure recording which variables are
       fixed; at the begining: none of them *)
  let init_fixed = {
      asTrueFalse   = Model.empty;
      justification = T2Clause.empty;
      propagation   = Pqueue.empty;
      unfolds       = Pqueue.empty;
      clauses          = TSet.empty;
      seen          = TSet.empty
    }

  (* Simplifying a clause according to the current fixed *)
  let simplify fixed = Constraint.simplify fixed.asTrueFalse
        
  (*******************************************************************)
  (* These are the ingredients to feed the propagate literals module *)
  (*******************************************************************)


  (* Type declarations needed for the 2-watched literals functor.
     When we stop because we have found UNSAT, we will provide:
     - a list of ThStraight messages,
     which are our reasoning steps towards the contradiction,
     - the unsat message which is the contradiction
     - the term representing the unsat clause
   *)
  type stop = straight list * ((sign,TSet.t,unsat) message)
    
  (* type used in the following function *)
  type result =
    | UNSAT     of stop
    | Propagate of fixed * LitF.t list

                                  
  (* This function takes a term that is a unit clause (according to justif),
     with l as its only literal.
     It creates the propagation data. *)

  let unit_base term justif l fixed =
    Dump.print ["bool",2] (fun p->
        p "unit called on %a" Term.print_in_fmt term);

    (* We set l to true and its negation nl to false, generating the
       term litterm to propagate. That term is pushed in the
       propagation queue, and mapped to the unit clause in the
       justification map. *)
    let litterm,asTrueFalse = Model.add l fixed.asTrueFalse in
    litterm,
    let clauses  = match (proj(Terms.data litterm)).asclause with
      | Some lset when LSet.info lset > 1
        -> TSet.add litterm fixed.clauses
      | _ -> fixed.clauses
    in
    let fixed = { fixed with
                  asTrueFalse = asTrueFalse;
                  clauses     = clauses;
                  seen        = TSet.add litterm fixed.seen } in
    if Terms.equal litterm term then fixed
    else
      (* There is a real unit propagation that we learn,
           we create the uc_clause, we update asTrueFalse,
           we record that litterm is there because of the uc_clause,
           and is something that the outside world needs to propagate *)
      let uc_clause = { term = term; simpl = Some l ; justif = justif } in
      {
        fixed with
        justification = T2Clause.add litterm uc_clause fixed.justification;
        propagation   = Pqueue.push litterm fixed.propagation
      }    


  let rec unit term ?(justif=[]) l ?(todo=[]) ?(tofix=[]) fixed =

    (* Finally, we tell the 2-watched literal module to mark 
       litterm and its negation as fixed *)
    let litterm,fixed = unit_base term justif l fixed in

    (* Now is this litterm actually a conjunction?
       It suffices to look at its field nasclause (negation as clause) *)
    match (proj(Terms.data litterm)).nasclause with
    | Some set when LSet.info set > 1
      ->
       (* It is a conjunction, we need to satisfy the conjuncts.
          We first compute the set of conjuncts *)

       let tset,mytodo =
         LSet.fold
           (fun l (tset,todo) ->
             let newterm = litAsTerm(LitF.negation l) in
             TSet.add newterm tset, newterm::todo )
           set
           (TSet.empty,todo)
       in
       Dump.print ["bool",2] (fun p->
           p "Unfold: %a\nfrom %a" TSet.print_in_fmt tset Term.print_in_fmt term);
       (* Forming the message that litterm entails the conjuncts *)
       let msg = straight () (TSet.singleton litterm) tset in
       (* We return the fixed with the non-obviously true conjuncts added,
               and the thStraight message queued *)
       run mytodo tofix { fixed with unfolds = Pqueue.push msg fixed.unfolds }

    | _ -> (* It's not a conjunction. Nothing to do. *)
       run todo tofix fixed
       
  and run mytodo tofix fixed =
    match mytodo with
    | []         -> Propagate(fixed,tofix)
    | term::todo -> treat term todo tofix fixed     
                
  (* This is the initialisation:
     we indicate to kernel that a new term is in town.
     Please record it as seen. As a literal, set it to true if possible. *)

  and treat term todo tofix fixed =
    Dump.print ["bool",2] (fun p->
        p "adding term %a" Term.print_in_fmt term);

    let fixed = { fixed with seen = TSet.add term fixed.seen } in
    let l     = (proj(Terms.data term)).aslit in
    let asTrue, asFalse = Model.reveal fixed.asTrueFalse in

    if LMap.mem l asFalse then
      (* Literal already false, must raise a conflict *)
      let otherterm = LMap.find l asFalse in
      (Dump.print ["bool",2] (fun p->
           p "term as lit already set to false because of %a"
             Term.print_in_fmt otherterm);
       let tset = TSet.singleton otherterm in
       let msgs,_ = explain_relevant tset fixed.justification in
       UNSAT(List.rev_append msgs [], unsat () (TSet.add term tset)))

    else if LMap.mem l asTrue then
      (* Literal already true, nothing to see here. *)
      (Dump.print ["bool",2] (fun p->
           p "term as lit already set to true because of %a"
             Term.print_in_fmt (LMap.find l asTrue));
       run todo tofix fixed)

    else
      (* As a literal, term is undetermined. Must set it to true. *)
      (Dump.print ["bool",2] (fun p->
           p "term as lit being set to true");
       let tofix = l::(LitF.negation l)::tofix in
       unit term l ~tofix ~todo fixed )


  let fix term fixed = treat term [] [] fixed
                              
  (* solve_clause fixed term slitterms
     A clause is justified as being true, under the proviso that slitterm is.
     We remove term from clauses. *)

  let solve_clause fixed term = function
    | Some litterm when not(TSet.mem litterm fixed.seen)
      -> failwith "bool: you are cheating"
    | _ ->
       if TSet.mem term fixed.clauses
       then { fixed with clauses = TSet.remove term fixed.clauses }
       else fixed

  (* constreat c fixed
     is called when the 2-watched literals module can no longer find 2 lits to watch
     in c (according to fixed), meaning that c is unit or constant. 3 cases occur:
       - the clause has become true
       - the clause has become empty (=false)
       - the clause has become unit
   *)
  let constreat c fixed =
    let term = Constraint.term c in
    Dump.print ["bool",0] (fun p->
        p "Adding constraint: %a" Term.print_in_fmt term );
    match Constraint.simpl c with

    | Sums.Case2 slitterm ->
       (* The clause is satisfied. We call solve_clause. *)
       
       Dump.print ["bool",2] (fun p->p "Clause is satisfied");
       Propagate(solve_clause fixed term slitterm, [])

    | Sums.Case1(set,justif) -> begin
        match LSet.reveal set with
        | Empty ->
           (* The clause has become empty/unsat. We collect in tset
           the terms that have been used to simplify it to empty, then
           we recursively collect the propagations messages that led
           to the conflict and haven't been communicated yet. *)

           Dump.print ["bool",2] (fun p->p "Clause is unsat");
           let tset = explain { term = term; simpl = None ; justif = justif } in
           let msgs,_ = explain_relevant tset fixed.justification in
           UNSAT(List.rev_append msgs [], unsat () tset)

        | Leaf(l,()) ->
           (* The clause has become unit, the last literal being l. 
              We call unit. *)
           unit term ~justif l fixed

        | Branch(_,_,_,_) ->
           failwith "bool: not supposed to see 2 literals in a clause triggerd for UP or conflict"
      end

                                  
  (* The is the type of data we want to extract from a fixed that we have saturated
     with our reasoning steps, but where we haven't found a contradiction:
     - either a message
     - or a demand of split, with a set of literals NOT to split on 
       (because already determined). *)
                                  
  type msg =
    | Msg : (sign,TSet.t,_) message -> msg
    | SplitBut : (Term.t,unit) LSet.param -> msg

  (* extract_msg constraints
     extracts from constraints the message declaring the propagations
     that have been performed *)

  let extract_msg fixed : (msg * fixed) option =
    Dump.print ["bool",1] (fun p->
        p "Starting msg extraction");
    match Pqueue.pop fixed.propagation, Pqueue.pop fixed.unfolds with

    | Some(litterm,propa),_ ->
       begin
         (* We form the thStraight message propagating litterm *)
         match formThStraight litterm fixed.justification with
         | None -> (* if litterm was in the queue, 
                      we must have recorded a justification for it, 
                      so the message forming should not fail *)
            failwith "Should not happen: extract_msg"
         | Some(term,_,msg,justif) ->
            let fixed = {fixed with justification = justif; propagation = propa} in
            Dump.print ["bool",1] (fun p->
                p "Found a message: %a" (print_msg_in_fmt TSet.print_in_fmt) msg);
            (* We return the message, together with the new fixed where
               the propagation has been popped and its justification removed,
               and the clause has been solved *)
            Some(Msg msg, solve_clause fixed term (Some litterm))
       end

    | None,Some(msg,unfolds) ->
       (* No more propagations, but some unfoldings to communicate *)
       Some(Msg msg, { fixed with unfolds = unfolds })

    | None,None ->
       (* Nothing to declare. Have all the clauses been solved? *)
       if TSet.is_empty fixed.clauses
       then (* if so, the Boolean theory has completed its model *)
         let msg = sat () fixed.seen in
         Some(Msg msg, fixed)
       else( (* if not, there must be literals that we should decide *)
         Dump.print ["bool",1] (fun p->
             p "clauses: %a" TSet.print_in_fmt fixed.clauses);
         (* We give our go-ahead for a split,
            but we need to compute the literals on which we forbid splitting
            (because they are already determined) *)
         let asTrue,asFalse = Model.reveal fixed.asTrueFalse in
         Dump.print ["bool",2] (fun p->
             let f =
               LMap.print_in_fmt 
                 (fun fmt (lit,_)->LitF.print_in_fmt fmt lit)
             in p "Is true: %a\nIs false: %a" f asTrue f asFalse
           );
         let lset = LMap.union
                      (fun _ _ -> failwith "extract_msg: shouldn't happen")
                      asTrue
                      asFalse
         in
         Some(SplitBut lset, fixed)
       )

  (* Plugin for Bool is asking us to produce a split message on lit *)
  let split lit =
    let t = litAsTerm lit in
    let nt = litAsTerm (LitF.negation lit) in
    both () TSet.empty (TSet.singleton t) (TSet.singleton nt)

  let clear() = LSet.clear();LMap.clear()

end

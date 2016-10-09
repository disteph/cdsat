open Top
open Messages
open Specs

open Prop.Literals

open General
open Patricia
open Patricia_interfaces
open SetConstructions

open MyStructures
       
type sign = unit
  
module Make(DS: sig 
                include GTheoryDSType
                val proj: Term.datatype -> ThDS.t
              end) = struct

  open DS
         
  (* Module for maps from literals to terms *)

  module DMap = struct
    type keys      = LitF.t
    let kcompare   = LitF.compare
    type values    = Term.t
    type infos     = unit
    let info_build = empty_info_build
    let treeHCons  = None (* Some(LitF.id,Terms.id,Terms.equal) *)
  end

  module LMap = PATMap.Make(DMap)(I)

  (* From a literal, give me the simplest term
     that is abstracted as that literal.
     Can only have 1 negation at the top-level. *)
                           
  let litAsTerm l =
    let b,i = LitF.reveal l in
    if b then Term.term_of_id i
    else Term.bC Symbols.Neg [Term.term_of_id i]

  (* Module for Boolean models. 
     We keep a bit more information than just "which lits are true/false",
     as we record 2 maps:
     - first map: maps a literal to a term whose assumption makes it true
     - second map: maps a literal to a term whose assumption makes it false
     If l is mapped to t in one map, (not l) is mapped to t in the other map.
     In (add l t), we create the simplest term, call it term, abstracted as l
     and add to the first map of t a mapping from l to term
     and to the second map of t a mapping from (not l) to term.
     We return the new pair of map, plus the term term that we created. *)
                           
  module Model : sig
      type t
      val reveal : t -> LMap.t*LMap.t
      val empty : t
      val add : LitF.t -> t -> (Term.t*t)
  end = struct
    type t       = LMap.t*LMap.t
    let reveal t = t
    let empty    = LMap.empty,LMap.empty
    let add l (t,tn) =
      Dump.print ["bool",2]
        (fun p->p "Setting this term to true: %a" Term.print_in_fmt (litAsTerm l));
      let ln = LitF.negation l in
      let dejavu = function
        | Some _ -> failwith "Literal already determined!"
        | None -> litAsTerm l
      in
      let t = LMap.add l dejavu t in
      let tn = LMap.add ln dejavu tn in
      LMap.find l t, (t,tn)
  end

  (*******************************************************************)
  (* These are the ingredients to feed the 2-watched literals module *)
  (*******************************************************************)

  (* Constraints are clauses, implemented as a record with 2 fields:
    - term: the original term representing the clause
    - simpl: represents the simplified version of the clause
             according to the current model,
             and is either Case1(lset, modelstack) or Case2(termoption).
      Case1(lset, modelstack) is for a clause, in which no literal is yet set to true.
      lset is the set of literals in the clause whose truth-value is undetermined,
      i.e. the literals in the clause that were set to false have been removed.
      modelstack is a stack of models:
      every time we have simplified the clause, we have pushed on the stack
      the model corresponding to the current trail, so as to keep
      track of why the clause was simplified in that way.
      Case2(termoption) is for a clause that has simplified to true.
      termoption is None if the clause was the true clause to start with
      or it is (Some term) if term is the term
      whose assumption in the trail makes the clause true *)

  module Constraint : sig
      include FromHConsed
      val make : Term.t -> t
      val term : t -> Term.t
      val simpl: t -> (LSet.t*(Model.t list), Term.t option) Sums.sum
      val verysimpl: t -> (LSet.t, Term.t option) Sums.sum
      val simplify : Model.t->t->t
      val print_in_fmt : Format.formatter -> t -> unit
  end = struct

    type t = {
        term : Term.t;
        simpl: (LSet.t*(Model.t list), Term.t option) Sums.sum;
      }

    let make t = {
        term = t;
        simpl =
          match (proj(Terms.data t)).asclause with
          | None      -> Sums.Case2 None
          | Some lset -> Sums.Case1(lset,[])               
      }

    let id c = Terms.id c.term
    let term  c = c.term
    let simpl c = c.simpl
    let verysimpl c = match c.simpl with
      | Sums.Case1(lset,_) -> Sums.Case1 lset
      | Sums.Case2 s -> Sums.Case2 s

    (* simplify model c
       simplifies clause c according to the
       currently fixed literals, given as model *)

    let simplify model c =
      match c.simpl with

      (* If clause was already simplified to true, we do not touch it: *)
      | Sums.Case2 _ -> c

      (* If clause was not already simplified to true: *)
      | Sums.Case1(set,j) ->
         let astrue,asfalse = Model.reveal model in
         let inter = LMap.inter_poly (fun _ v () -> v) astrue set in
         if LMap.is_empty inter
         then
           (* If fixed literals do not make the clause true: *)

           let updated_clause = LSet.diff_poly set asfalse in
           { c with simpl = Sums.Case1(updated_clause, model::j ) }
         else
           (* If clause gets simplified to true: *)

           let _,term = LMap.choose inter in
           {c with simpl = Sums.Case2(Some term)}

    let print_in_fmt fmt t =
      Format.fprintf fmt "%a" Term.print_in_fmt t.term
  end
       
  (* Type for a clause that has become unit or false, given as: the
term representing it, the simplified form (None for the false clause,
Some l if remaining lit is l), and the stack of models used for this
simplification.  *)

  type uc_clause = {
      term : Term.t;
      simpl: LitF.t option;
      justif: Model.t list
    }
         
  (* Module for maps from literals (represented as terms) to clauses -
  used to record, when a literal has been propagated, which clause was
  used for the propagation *)
      
  module T2Clause = Map.Make(struct
                              type t = Term.t
                              let compare = Terms.compare
                            end)

  (* This is a type abbreviation for those propoagation messages that
  we will send to the outside world. *)

  type straight = (sign,TSet.t,Messages.straight) message

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
   - field orig_clauses:
original clauses that have not been satisfied yet
   - field true_clauses:
original clauses that have simplified to true
  *)

  type fixed = {
      asTrueFalse  : Model.t;
      justification: uc_clause T2Clause.t;
      propagation  : Term.t Pqueue.t;
      unfolds      : straight Pqueue.t;
      orig_clauses : TSet.t;
      true_clauses : TSet.t
    }

  (* init_fixed is the initial structure recording which variables are
       fixed; at the begining: none of them *)
  let init_fixed = {
      asTrueFalse   = Model.empty;
      justification = T2Clause.empty;
      propagation   = Pqueue.empty;
      unfolds       = Pqueue.empty;
      orig_clauses  = TSet.empty;
      true_clauses  = TSet.empty
    }

  (* Simplifying a clause according to the current fixed *)
  let simplify fixed = Constraint.simplify fixed.asTrueFalse

                                                  
  (***********************************************************)
  (* This is the generation of explanations, for e.g. conflict
  analysis *)
  (***********************************************************)

                      
  (* Input : a uc_clause 
     Output: extract from its stack of models the relevant part that
has turned clause unit or false, expressing it as a TSet and
including the term representing the clause (that TSet can directly be
used in a ThStraight or ThProvable message. *)

  let explain (clause : uc_clause) : TSet.t =
    match (proj(Terms.data clause.term)).asclause with
    | None -> failwith "Clause should not be trivially true"
    | Some original ->
       let inc = function
         | Empty, _ -> true
         | Leaf(l,()), Some l' when LitF.compare l l' == 0 -> true
         | _ -> false
       in       
       let rec analyse set stack res =
         match LSet.reveal set, stack with

         | Empty, _ | Leaf _, _  when inc (LSet.reveal set, clause.simpl) ->
            LMap.fold (fun _ -> TSet.add) res (TSet.singleton clause.term)

         | _, model::tail ->
            let _,asfalse = Model.reveal model in
            let res' = LMap.inter_poly (fun _ v () -> v) asfalse set in
            let set' = LSet.diff_poly set asfalse in
            analyse set' tail (LMap.union (fun v _ -> v) res' res)

         | _ -> failwith "Should not happen"
       in
       analyse original clause.justif LMap.empty

  (* This is used to form a ThStraight message to the outside world,
     in order to unit propagate litterm from a certain set to be extracted from justif. *)

  let formThStraight (litterm:Term.t) justif
      : (Term.t * TSet.t * straight * (uc_clause T2Clause.t)) option =
    if not(T2Clause.mem litterm justif)
    then None
    else
      (* Getting from justif the unit clause that allows propagation of litterm *)
      let uc_clause = T2Clause.find litterm justif in
      (* Extracting the set of terms that were used to make this clause unit *)
      let tset = explain uc_clause in
      (* Forming the propagation message (tset |- litterm) *)
      let msg = straight () tset (TSet.singleton litterm) in
      (* Now that the message is formed, we remove litterm from justif *)
      let justif' = T2Clause.remove litterm justif in
      (* We return the unit clause, the tset allowing the propagation, 
         the message, and the cleaned up justif *)
      Some(uc_clause.term,tset,msg,justif')

  (* In (explain_relevant relevant justif),
     relevant is a set of terms that were internally added to the trail.
     We want to form the list of ThStraight messages that we need to communicate to 
     the outside world so that it cathes up with our trail.
   *)
          
  let explain_relevant relevant (justif : uc_clause T2Clause.t)
      : (straight list) * (uc_clause T2Clause.t) =
    (* Function treat just explains 1 term: litterm,
       before calling itself recursively.
       msgs is the list of messages we have accumulated so far,
       justif is the justification structure that we have simplified so far. *)
    let rec treat litterm ((msgs,justif) as sofar) =
      (* First, we try to form the ThStraight message that "unit propagates" litterm *)
      match formThStraight litterm justif with
      | None ->        
         (* If we got None, it means that justif does not explain why
            we have litterm in the trail, which means that the outside
            world already has litterm in its trail. Nothing to do. *)
         sofar
      | Some(_,tset,msg,justif) ->
         (* OK, so we can explain litterm with msg, which says (tset|-litterm),
            but now we must explain tset. This is what we do recursively. *)
         let msgs,justif = TSet.fold treat tset (msgs,justif) in
         Dump.print ["bool",3] (fun p->
             p "Generating %a" (print_msg_in_fmt TSet.print_in_fmt) msg);
         (* Finally, we add msg to the list *)
         msg::msgs, justif
    in
    (* For each term in relevant, we call treat: the argument ([],justif)
       contains [], the initial list where we will accumulate ThStraight messages,
       and justif, that we will consume with every ThStraight message that we form. *)
    TSet.fold treat relevant ([],justif)

        
  (*******************************************************************)
  (* These are the ingredients to feed the propagate literals module *)
  (*******************************************************************)

  let solve_clause term slitterm fixed =
    (* Assume term encodes a clause that is satisfied by slitterm. If
       the clause was in orig_clauses, we remove it; in any case we
       add it to true_clauses together with the literal term that
       makes the clause true. *)
    Dump.print ["bool",2]
      (fun p-> p "Solve_clause:term=%a\nslitterm=%a"
                 Term.print_in_fmt term
                 (fun fmt slitterm ->
                   match slitterm with
                   | None -> ()
                   | Some litterm -> Format.fprintf fmt "%a" Term.print_in_fmt litterm
                 )
                 slitterm
      );
    (* Clause that is made true is removed from the list of clauses to satisfy *)
    let orig_clauses =
      if TSet.mem term fixed.orig_clauses
      then TSet.remove term fixed.orig_clauses
      else fixed.orig_clauses
    in
    match slitterm with
    | Some litterm ->
       (* Clause term is made true because of litterm.
          We add them both to true_clauses *)
       
       let true_clauses = TSet.add litterm (TSet.add term fixed.true_clauses) in
       begin
         (* Now is this litterm actually a conjunction?
            It suffices to look at its field nasclause (negation as clause) *)
         
         match (proj(Terms.data litterm)).nasclause with
         | Some set when LSet.info set > 1
           ->
            (* It is a conjunction, we need to satisfy the conjuncts.
               We first compute the set of conjuncts *)

            let tset = LSet.fold
                         (fun l -> TSet.add (litAsTerm(LitF.negation l)))
                         set
                         TSet.empty
            in
            Dump.print ["bool",2] (fun p->
                p "Unfold: %a\nfrom %a" TSet.print_in_fmt tset Term.print_in_fmt term);
            (* Forming the message that litterm entails the conjuncts *)
            let msg = straight () (TSet.singleton litterm) tset in
            (* We return the fixed with the non-obviously true conjuncts added,
               and the thStraight message queued *)
            { fixed with
              orig_clauses = TSet.diff(TSet.union orig_clauses tset) true_clauses;
              unfolds      = Pqueue.push msg fixed.unfolds;
              true_clauses = true_clauses }

         | Some _ -> (* It's not a conjunction. Nothing to do. *)
            { fixed with
              orig_clauses = orig_clauses;
              true_clauses = true_clauses }

         | None -> (* Negation of litterm is true clause, 
                      so it means litterm is the constant false *)
            Dump.print ["bool",2] (fun p-> p "negation of litterm is true clause");
            let msg = straight () (TSet.singleton litterm) (TSet.singleton(Term.bC Symbols.False [])) in
            { fixed with
              orig_clauses = orig_clauses;
              unfolds      = Pqueue.push msg fixed.unfolds;
              true_clauses = true_clauses }
       end
         
    | None -> { fixed with
                orig_clauses = orig_clauses;
                true_clauses = TSet.add term fixed.true_clauses }

        
    


  (* Type declarations needed for the 2-watched literals functor.
     When we stop because we have found UNSAT, we will provide:
     - a list of ThStraight messages,
     which are our reasoning steps towards the contradiction,
     - the unsat message which is the contradiction
     - the term representing the unsat clause
   *)
  type stop = straight list * ((sign,TSet.t,unsat) message) * Term.t
    
  (* type used in the following function *)
  type result =
    | UNSAT     of stop
    | Propagate of fixed * LitF.t list
    | Meh       of fixed

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
       Meh(solve_clause term slitterm fixed)

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
           UNSAT(List.rev_append msgs [], unsat () tset, term)

        | Leaf(l,()) ->
           (* The clause has become unit, the last literal being l. We
           set l to true and its negation nl to false, generating the
           term litterm to propagate. That term is pushed in the
           propagation queue, and mapped to the unit clause in the
           justification map. *)

           let litterm,asTrueFalse = Model.add l fixed.asTrueFalse in
           Dump.print ["bool",2] (fun p->
               p "Clause is unit on %a" Term.print_in_fmt litterm);
           let fixed =
             if Terms.equal litterm term
             then
               (* We were in presence of a unit clause from the start,
                  we simply call solve_clause, 
                  as litterm may be a conjunction to unfold  *)
               solve_clause term (Some litterm)
                    { fixed with asTrueFalse = asTrueFalse }
             else
               (* There is a real unit propagation that we learn,
                  we create the uc_clause, we update asTrueFalse,
                  we record that litterm is there because of the uc_clause,
                  and is something that the outside world needs to propagate *)
               let uc_clause = { term = term; simpl = Some l ; justif = justif } in
               {
                 fixed with
                 asTrueFalse   = asTrueFalse;
                 justification = T2Clause.add litterm uc_clause fixed.justification;
                 propagation   = Pqueue.push litterm fixed.propagation
               }
           in
           (* Finally, we tell the 2-watched literal module to mark 
              litterm and its negation as fixed *)
           Propagate(fixed, [l; LitF.negation l])

        | Branch(_,_,_,_) -> failwith "bool: not supposed to see 2 literals in a clause triggerd for UP or conflict"

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
            Some(Msg msg, solve_clause term (Some litterm) fixed)
       end

    | None,Some(msg,unfolds) ->
       (* No more propagations, but some unfoldings to communicate *)
       Some(Msg msg, { fixed with unfolds = unfolds })

    | None,None ->
       (* Nothing to declare. Have all the clauses been solved? *)
       if TSet.is_empty fixed.orig_clauses
       then (* if so, the Boolean theory has completed its model *)
         let msg = sat () fixed.true_clauses in
         Some(Msg msg, fixed)
       else( (* if not, there must be literals that we should decide *)
         Dump.print ["bool",1] (fun p->
             p "orig_clauses: %a" TSet.print_in_fmt fixed.orig_clauses);
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

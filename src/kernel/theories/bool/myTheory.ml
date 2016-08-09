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

  (* Module for those maps mapping literals to a term that makes them
  false *)

  let litAsTerm l =
    let b,i = LitF.reveal l in
    if b then Term.term_of_id i
    else Term.bC Symbols.Neg [Term.term_of_id i]

                           
  module L2Term : sig
      type t
      val reveal : t -> LMap.t*LMap.t
      val empty : t
      val add : LitF.t -> t -> (Term.t*t)
  end = struct
    type t = LMap.t*LMap.t
    let reveal t = t
    let empty = LMap.empty,LMap.empty
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

  (* Constraints are clauses, made of 
    - term: the original term representing the clause
    - simpl: either None (if clause is trivially true) or
      Some(set,maplist), where set is the set of clause literals that
      remain undetermined and an element map of maplist is a map whose
      keys are literals set to false and values are the input terms
      whose assertion made these literals false *)

  module Constraint : sig
      include FromHConsed
      val make : Term.t -> t
      val term : t -> Term.t
      val simpl: t -> (LSet.t*(L2Term.t list), Term.t option) Sums.sum
      val verysimpl: t -> (LSet.t, Term.t option) Sums.sum
      val simplify : L2Term.t->t->t
    end = struct

    type t = {
        term : Term.t;
        simpl: (LSet.t*(L2Term.t list), Term.t option) Sums.sum;
      }

    let make t = {
        term = t;
        simpl =
          match (proj(Terms.data t)).asclause with
          | None -> Sums.F(None)
          | Some lset -> Sums.A(lset,[])               
      }
                   
    let id c = Terms.id c.term
    let term  c = c.term
    let simpl c = c.simpl
    let verysimpl c = match c.simpl with
      | Sums.A(lset,_) -> Sums.A lset
      | Sums.F s -> Sums.F s

    (* simplify is used to simplify a clause according to the
      currently fixed literals *)

    let simplify l2term c =
      match c.simpl with

      (* If clause was already simplified to true: *)
      | Sums.F _ -> c

      | Sums.A(set,j) ->
         let astrue,asfalse = L2Term.reveal l2term in
         let inter = LMap.inter_poly (fun _ v () -> v) astrue set in
         if LMap.is_empty inter
         then
           (* If fixed literals do not make the clause true: *)

           let updated_clause = LSet.diff_poly set asfalse in
           { c with simpl = Sums.A(updated_clause, l2term::j ) }
         else
           (* If clause gets simplified to true: *)

           let _,term = LMap.choose inter in
           {c with simpl = Sums.F(Some term)}
             
  end
       
  (* Type for a clause that has become unit or constant, given as: the
term representing it, the simplified form (None for constant clause,
Some l if remaining lit is l), and the stack of models used for this
simplification.  *)

  type uc_clause = {
      term : Term.t;
      simpl: LitF.t option;
      justif: L2Term.t list
    }
         
  (* Module for maps from literals to clauses - used to record, when a
  literal has been propagated, which clause was used for the
  propagation *)
      
  module T2Clause = Map.Make(struct
                              type t = Term.t
                              let compare = Terms.compare
                            end)

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
      asTrueFalse  : L2Term.t;
      justification: uc_clause T2Clause.t;
      propagation  : Term.t Pqueue.t;
      unfolds      : straight Pqueue.t;
      orig_clauses : TSet.t;
      true_clauses : TSet.t
    }

  (* init_fixed is the initial structure recording which variables are
       fixed; at the begining: none of them *)
  let init_fixed = {
      asTrueFalse   = L2Term.empty;
      justification = T2Clause.empty;
      propagation   = Pqueue.empty;
      unfolds       = Pqueue.empty;
      orig_clauses  = TSet.empty;
      true_clauses  = TSet.empty
    }


  let simplify fixed = Constraint.simplify fixed.asTrueFalse

                                                  
  (***********************************************************)
  (* This is the generation of explanations, for e.g. conflict
  analysis *)
  (***********************************************************)

                      
  (* Input : a uc_clause 
     Output: extract from the stack of models the relevant part that
has turned clause unit or constant, expressing it as a TSet and
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

         | _, l2term::tail ->
            let _,asfalse = L2Term.reveal l2term in
            let res' = LMap.inter_poly (fun _ v () -> v) asfalse set in
            let set' = LSet.diff_poly set asfalse in
            analyse set' tail (LMap.union (fun v _ -> v) res' res)

         | _ -> failwith "Should not happen"
       in
       
       analyse original clause.justif LMap.empty

  let formThStraight (litterm:Term.t) justif
      : (Term.t * TSet.t * straight * (uc_clause T2Clause.t)) option =
    if not(T2Clause.mem litterm justif)
    then None
    else
      let clause = T2Clause.find litterm justif in
      let tset = explain clause in
      let msg = straight () tset (TSet.singleton litterm) in
      let justif' = T2Clause.remove litterm justif in
      Some(clause.term,tset,msg,justif')

  let explain_relevant relevant (justif : uc_clause T2Clause.t)
      : (straight list) * (uc_clause T2Clause.t) =
    let rec treat litterm ((msgs,justif) as sofar) =
      match formThStraight litterm justif with
      | None -> sofar
      | Some(_,tset,msg,justif) ->
         let msgs,justif = TSet.fold treat tset (msgs,justif) in
         Dump.print ["bool",3] (fun p->p "Generating %a" (print_msg_in_fmt TSet.print_in_fmt) msg);
         msg::msgs, justif
    in
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
    let orig_clauses =
      if TSet.mem term fixed.orig_clauses
      then TSet.remove term fixed.orig_clauses
      else fixed.orig_clauses
    in
    match slitterm with
    | Some litterm ->
       let true_clauses = TSet.add litterm (TSet.add term fixed.true_clauses) in
       begin
         match (proj(Terms.data litterm)).nasclause with
         | Some set when LSet.info set > 1
           ->

            let tset = LSet.fold
                         (fun l -> TSet.add (litAsTerm(LitF.negation l)))
                         set
                         TSet.empty
            in
            Dump.print ["bool",2]
              (fun p-> p "Unfold: %a\nfrom %a" TSet.print_in_fmt tset Term.print_in_fmt term);

            let msg = straight () (TSet.singleton litterm) tset in
            { fixed with
              orig_clauses = TSet.diff(TSet.union orig_clauses tset) true_clauses;
              unfolds      = Pqueue.push msg fixed.unfolds;
              true_clauses = true_clauses }

         | Some _ -> 
            { fixed with
              orig_clauses = orig_clauses;
              true_clauses = true_clauses }

         | None ->
            Dump.print ["bool",2]
              (fun p-> p "negation of slitterm is true clause");
            let msg = straight () (TSet.singleton litterm) (TSet.singleton(Term.bC Symbols.False [])) in
            { fixed with
              orig_clauses = orig_clauses;
              unfolds      = Pqueue.push msg fixed.unfolds;
              true_clauses = true_clauses }
       end
         
    | None -> { fixed with
                orig_clauses = orig_clauses;
                true_clauses = TSet.add term fixed.true_clauses }

        
    


  (* Type declarations needed for the 2-watched literals functor *)
  type stop = straight list * ((sign,TSet.t,unsat) message) * Term.t
    
  (* type used in the following function *)
  type result =
    | UNSAT     of stop
    | Propagate of fixed * LitF.t list
    | Meh   of fixed

  (* constreat constraint
       treats the addition of constraint (=clause).
       4 cases occur:
       - the clause has become true
       - the clause has become empty (=false)
       - the clause has become unit
   *)
  let constreat c fixed =
    let term = Constraint.term c in
    if TSet.mem term fixed.true_clauses
    then Meh fixed
    else
      begin
        Dump.print ["bool",2]
          (fun p->p "Adding constraint: %a" Term.print_in_fmt term );
        match Constraint.simpl c with

        | Sums.F slitterm ->
           (* The clause is satisfied. If it was in orig_clauses, we
       remove it; in any case we add it to true_clauses together with
       the literal term that makes the clause true. *)
           
           Dump.print ["bool",2] (fun p->p "Clause is satisfied");
           Meh(solve_clause term slitterm fixed)

        | Sums.A(set,justif) -> begin
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

               let litterm,asTrueFalse = L2Term.add l fixed.asTrueFalse in
               Dump.print ["bool",2] (fun p->p "Clause is unit on %a" Term.print_in_fmt litterm);
               let fixed =
                 if Terms.equal litterm term
                 then solve_clause term (Some litterm) { fixed with asTrueFalse = asTrueFalse }
                 else
                   let uc_clause = { term = term; simpl = Some l ; justif = justif } in
                   {
                     fixed with
                     asTrueFalse   = asTrueFalse;
                     justification = T2Clause.add litterm uc_clause fixed.justification;
                     propagation   = Pqueue.push litterm fixed.propagation
                   }
               in
               Propagate(fixed, [l; LitF.negation l])

            | Branch(_,_,_,_) -> failwith "bool: not supposed to see 2 literals in a clause triggerd for UP or conflict"

          end
      end

  type msg =
    | Msg : (sign,TSet.t,_) message -> msg
    | SplitBut : (Term.t,unit) LSet.param -> msg

  (* extract_msg constraints
     extracts from constraints the message declaring the propagations
     that have been performed *)

  let extract_msg fixed : (msg * fixed) option =
    Dump.print ["bool",1]
      (fun p-> p "Starting msg extraction");
    match Pqueue.pop fixed.propagation, Pqueue.pop fixed.unfolds with

    | Some(litterm,propa),_ ->
       begin
         match formThStraight litterm fixed.justification with
         | None -> failwith "Should not happen: extract_msg"
         | Some(term,_,msg,justif) ->
            let fixed = {fixed with justification = justif; propagation = propa} in            
            Dump.print ["bool",1]
              (fun p-> p "Found a message: %a" (print_msg_in_fmt TSet.print_in_fmt) msg);
            Some(Msg msg, solve_clause term (Some litterm) fixed)
       end

    | None,Some(msg,unfolds) ->
       Some(Msg msg, { fixed with unfolds = unfolds })

    | None,None ->
       if TSet.is_empty fixed.orig_clauses
       then
         let msg = sat () fixed.true_clauses in
         Some(Msg msg, fixed)
       else(
         Dump.print ["bool",2]
           (fun p-> p "orig_clauses: %a" TSet.print_in_fmt fixed.orig_clauses);
         let asTrue,asFalse = L2Term.reveal fixed.asTrueFalse in
         Dump.print ["bool",2]
           (fun p-> let f =
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

        
  let split lit =
    let t = litAsTerm lit in
    let nt = litAsTerm (LitF.negation lit) in
    both () TSet.empty (TSet.singleton t) (TSet.singleton nt)

  let clear() = LSet.clear();LMap.clear()

end

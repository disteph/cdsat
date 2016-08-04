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
  
module Make
  (DS: sig 
    include GTheoryDSType
    val proj: Term.datatype -> ThDS.t
  end) =
struct

  open DS
         
  (* Module for maps from literals to terms *)

  module DMap = struct
    type keys      = LitF.t
    let kcompare   = LitF.compare
    type values    = Term.t
    type infos     = unit
    let info_build = empty_info_build
    let treeHCons  = Some(LitF.id,Terms.id,Terms.equal)
  end

  module LMap = PATMap.Make(DMap)(I)

  (* Module for those maps mapping literals to a term that makes them
  false *)

  let litAsTerm l =
    let b,i = LitF.reveal l in
    if b then Term.term_of_id i
    else Term.bC Symbols.Neg [Term.term_of_id i]

                           
  module L2Term =
    (struct
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
    end : sig
      type t
      val reveal : t -> LMap.t*LMap.t
      val empty : t
      val add : LitF.t -> t -> (Term.t*t)
    end)

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

  module Constraint =
    (struct

      type t = {
          term : Term.t;
          simpl: (LSet.t*(L2Term.t list), Term.t option) Sums.sum;
        }

      let make t = {
          term = t;
          simpl =
            let o,_ = proj(Terms.data t) in
            match o with
            | None -> Sums.F(None)
            | Some lset -> Sums.A(lset,[])               
        }
                     
      let id c = Terms.id c.term
      let term  c = c.term
      let simpl c = c.simpl

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

                 
    end : sig
      include FromHConsed
      val make : Term.t -> t
      val term : t -> Term.t
      val simpl: t -> (LSet.t*(L2Term.t list), Term.t option) Sums.sum
      val simplify : L2Term.t->t->t
    end)

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
      orig_clauses : TSet.t;
      true_clauses : TSet.t
    }

  (* init_fixed is the initial structure recording which variables are
       fixed; at the begining: none of them *)
  let init_fixed = {
      asTrueFalse   = L2Term.empty;
      justification = T2Clause.empty;
      propagation   = Pqueue.empty;
      orig_clauses  = TSet.empty;
      true_clauses  = TSet.empty
    }


  let simplify fixed = Constraint.simplify fixed.asTrueFalse
                     
  (* Given a clause, together with a set of fixed lits and a lit,
       pick another lit to watch *)
                                 
  let pick_another _ (c :Constraint.t) (var :LitF.t) : LitF.t option =

    match Constraint.simpl c with

    (* If clause not already true *)
    | Sums.A(set,_) ->
       let tochoose = 
         if LSet.mem var set
         then LSet.remove var set
         else set
       in
       if LSet.is_empty tochoose
       then None
       else Some(LSet.choose tochoose)

    (* If clause is already true: *)
    | Sums.F _ -> None


  (* Type declarations needed for the 2-watched literals functor *)
  type straight = (sign,TSet.t,Messages.straight) message
                    
  type msg =
    | Msg : (sign,TSet.t,_) message -> msg
    | SplitBut : (Term.t,unit) LSet.param -> msg

  type stop = straight list * ((sign,TSet.t,unsat) message) * Term.t

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
    let o,_ = proj(Terms.data clause.term) in
    match o with
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

    let orig_clauses =
      if TSet.mem term fixed.orig_clauses
      then TSet.remove term fixed.orig_clauses
      else fixed.orig_clauses
    in
    let true_clauses =
      match slitterm with
      | Some litterm -> TSet.add litterm fixed.true_clauses
      | None -> fixed.true_clauses
    in
    { fixed with
      orig_clauses = orig_clauses;
      true_clauses = TSet.add term true_clauses }


  (* extract_msg constraints
     extracts from constraints the message declaring the propagations
     that have been performed *)

  let extract_msg fixed : (msg * fixed) option =
    match Pqueue.pop fixed.propagation with
    | Some(litterm,propa) ->
       begin
         match formThStraight litterm fixed.justification with
         | None -> failwith "Should not happen: extract_msg"
         | Some(term,_,msg,justif) ->
            let fixed = {fixed with justification = justif; propagation = propa} in            
            Some(Msg msg, solve_clause term (Some litterm) fixed)
       end
    | None ->
       if TSet.is_empty fixed.orig_clauses
       then
         let msg = sat () fixed.true_clauses in
         Some(Msg msg, fixed)
       else
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

    
  (* type used in the following function *)
  type result =
    | UNSAT     of stop
    | Propagate of fixed * LitF.t list
    | Meh   of fixed
    | Watch of fixed * LitF.t * LitF.t

  (* constreat constraint
       treats the addition of constraint (=clause).
       4 cases occur:
       - the clause has become true
       - the clause has become empty (=false)
       - the clause has become unit
       - the clause has at least 2 undetermined literals, which we can watch
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
               (* The clause has becom unit, the last literal being l. We
           set l to true and its negation nl to false, generating the
           term litterm to propagate. That term is pushed in the
           propagation queue, and mapped to the unit clause in the
           justification map. *)

               Dump.print ["bool",2] (fun p->p "Clause is unit on %a" Term.print_in_fmt (litAsTerm l));
               let litterm,asTrueFalse = L2Term.add l fixed.asTrueFalse in
               let fixed =
                 if Terms.equal litterm term
                 then { fixed with asTrueFalse = asTrueFalse;
                                   true_clauses = TSet.add term fixed.true_clauses }
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

            | Branch(_,_,set1,set2) ->
               (* The clause is neither true not unsat nor unit: we can
           pick 2 literals to watch. We record it as a clause that
           remains to be satisfied. *)

               Dump.print ["bool",2] (fun p->p "Clause is now watched");
               let fixed = { fixed with orig_clauses = TSet.add term fixed.orig_clauses} in
               Watch(fixed, LSet.choose set1, LSet.choose set2)
          end
      end
        
  let split lit =
    let t = litAsTerm lit in
    let nt = litAsTerm (LitF.negation lit) in
    both () TSet.empty (TSet.singleton t) (TSet.singleton nt)

  let unfold term =
    match proj(Terms.data term) with
    | _, Some set when LSet.info set > 1 ->
       let tset = LSet.fold (fun l -> TSet.add (litAsTerm(LitF.negation l))) set TSet.empty in
       Dump.print ["bool",2]
         (fun p-> p "Unfold: %a\nfrom %a" TSet.print_in_fmt tset Term.print_in_fmt term);
       Some(straight () (TSet.singleton term) tset)
    | _ -> None
          
end

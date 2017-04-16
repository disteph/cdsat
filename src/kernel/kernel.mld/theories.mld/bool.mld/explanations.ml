open Top
open Messages
open Specs

open General
open Patricia_interfaces

open Termstructures
open Literals
open Clauses
         
module Make(DS: sig 
                include GTheoryDSType
                val proj: Term.datatype -> Clauses.TS.t
              end) = struct

  open DS
         
  include Basis.Make(DS)

  (* Type for a clause that has become unit or false, given as: the
term representing it, the simplified form (None for the false clause,
Some l if remaining lit is l), and the stack of models used for this
simplification.  *)

  type uc_clause = {
      term : Term.t;
      info : (LitF.t option * Model.t list, TSet.t) Sums.sum
    }
                     
  (* Module for maps from literals (represented as terms) to clauses -
  used to record, when a literal has been propagated, which clause was
  used for the propagation *)
                     
  module T2Clause = Map.Make(Term)

  (* This is a type abbreviation for those propoagation messages that
  we will send to the outside world. *)

  type straight = (unit,TSet.t,Messages.straight) message

                                                  
  (***********************************************************)
  (* This is the generation of explanations, for e.g. conflict
  analysis *)
  (***********************************************************)

                      
  (* Input : a uc_clause 
     Output: extract from its stack of models the relevant part that
has turned clause unit or false, expressing it as a TSet and
including the term representing the clause (that TSet can directly be
used in a ThStraight or ThProvable message. *)

  let explain term slit modelstack =
    match (proj(Terms.data term)).asclause with
    | None -> failwith "Clause should not be trivially true"
    | Some original ->
       let inc = function
         | Empty, _ -> true
         | Leaf(l,()), Some l' when LitF.equal l l' -> true
         | _ -> false
       in       
       let rec analyse set stack res =
         match LSet.reveal set, stack with

         | Empty, _ | Leaf _, _  when inc (LSet.reveal set, slit) ->
            LMap.fold (fun _ -> TSet.add) res (TSet.singleton term)

         | _, model::tail ->
            let _,asfalse = Model.reveal model in
            let res' = LMap.inter_poly (fun _ v () -> v) asfalse set in
            let set' = LSet.diff_poly set asfalse in
            analyse set' tail (LMap.union (fun v _ -> v) res' res)

         | _ -> failwith "Should not happen"
       in
       analyse original modelstack LMap.empty

               
  (* This is used to form a ThStraight message to the outside world,
     in order to unit propagate litterm from a certain set to be extracted from justif. *)

  let formThStraight litterm justif =
    if not(T2Clause.mem litterm justif)
    then None
    else
      (* Getting from justif the unit clause that allows propagation of litterm *)
      let uc_clause = T2Clause.find litterm justif in
      (* We remove litterm from justif *)
      let justif = T2Clause.remove litterm justif in

      (* Forming the propagation message (tset |- litterm) *)
      let msg = match uc_clause.info with
        | Sums.Case1(slit,modelstack) ->
           (* Extracting the set of terms that were used to make this clause unit or false *)
           let tset = explain uc_clause.term slit modelstack in
           straight () tset (TSet.singleton litterm)
        | Sums.Case2 conjuncts ->
           straight () (TSet.singleton uc_clause.term) conjuncts
      in

      (* We return the unit clause, the tset allowing the propagation, 
         the message, and the cleaned up justif *)
      Some(uc_clause.term,msg,justif)

  (* In (explain_relevant relevant justif),
     relevant is a set of terms that were internally added to the trail.
     We want to form the list of ThStraight messages that we need to communicate to 
     the outside world so that it cathes up with our trail.
   *)
          
  let explain_relevant relevant justif =
    Dump.print ["bool",3] (fun p->
        p "Extracting relevant propagations to get %a" TSet.print_in_fmt relevant);

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
         Dump.print ["bool",3] (fun p->
             p "%a should be already in the trail" Term.print_in_fmt litterm);
         sofar

      | Some(_,msg,justif) ->
         (* OK, so we can explain litterm with msg, which says (tset|-litterm),
            but now we must explain tset. This is what we do recursively. *)
         let Propa(tset,Straight _) = msg in
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


end

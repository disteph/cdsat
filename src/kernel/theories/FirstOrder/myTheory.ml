open Format

open Top
open Interfaces_basic
open Basic
open Symbol
open Specs

open Prop
open Literals
open Formulae
open Interfaces_theory

module IntSortSet = Set.Make(IntSort)

module IJMon = (struct
  type 'a t = 'a*int*int*IntSortSet.t
  let return a = (a,0,-1,IntSortSet.empty)
  let bind (f: 'a -> 'b t) (a,i,j,s)
      = let (b,i',j',s') = f a in
        (b,Pervasives.max i i',Pervasives.min j j',IntSortSet.union s s')
end : MonadType with type 'a t = 'a*int*int*IntSortSet.t)

let proj (a,_,_,_) = a

module Make(PropDS:DataType) = struct

  module DS = struct

    module DT = struct

      type t =
      | TermI : unit IJMon.t -> t
      | PropI : LitF.t IJMon.t -> t

      let bV is =
        match IntSort.reveal is with
        | _,Sorts.Prop -> PropI(LitF.build(true,is),0,-1,IntSortSet.add is IntSortSet.empty)
        | i,_ when i<0 -> TermI((),0,i,IntSortSet.empty)
        | i,_          -> TermI((),i,-1,IntSortSet.empty)

      let toLit = function
        | PropI f -> f
        | _       -> raise (ModelError "ModelError: trying to convert into a formula an expression that clearly is not one")

      let toTerm = function
        | TermI t -> t
        | _       -> raise (ModelError "ModelError: trying to convert into a term an expression that clearly is not one")

      let rec list_collect = function
        | []   -> IJMon.return ()
        | a::l -> IJMon.bind (fun () -> list_collect l) (toTerm a)

      let negation (a,i,j,s) = (LitF.negation a,i,j,s)

      let bC_aux symb l =
        match symb, l with
        | Neg,[l] -> negation l
        | _ -> raise (ModelError "ModelError: semantic_aux does not know symbol")

      let bC id symb l =
        (* print_endline(string_of_int id); *)
        try PropI(bC_aux symb (List.map toLit l))
        with ModelError _
          -> let ((),i,j,s) as domain = list_collect l in
             let (o,_) = Symbol.arity symb in
             match o with
             | Sorts.Prop -> PropI(LitF.build(true,IntSort.buildH(id,o)),i,j,s)
             | _          -> TermI domain
    end

    open DT

    module Term = Terms.Make(IntSort)(Pairing(PropDS)(DT))

    type formulae = PropDS.t
    let asF = fst

    let asL term = match snd(Terms.data term) with
      | PropI (l,i,j,s)  -> (* print_endline (Dump.toString (fun p -> p "Lit: %a" LitF.print_in_fmt l)); *)
        let b,is = LitF.reveal l in
                            let atom = 
                              if IntSort.isDefined is
                              then let (id,_) = IntSort.reveal is in
                                   Term.term_of_id id
                              else Term.bV is
                            in
                            (* print_endline (Dump.toString (fun p -> p "Term: %a" Term.print_in_fmt atom)); *)
                            Some(b,atom),i,j,s
      | TermI ((),i,j,s) -> None,i,j,s

    module OT = struct
      type t = Term.t
      let compare t1 t2 = match snd(Terms.data t1), snd(Terms.data t2) with
        | PropI l1, PropI l2 -> LitF.compare (proj l1) (proj l2)
        | PropI _, TermI _ -> 1
        | TermI _, PropI _ -> -1
        | TermI _, TermI _ -> 0
      let print_in_fmt fmt term =
        let res,i,j,s = asL term in
        match res with
        | None -> fprintf fmt "%a" Term.print_in_fmt term
        | Some(b,atom) -> fprintf fmt "%s%a" (if b then "" else "-") Term.print_in_fmt atom
    end

    module TSet = MakeCollection(struct include Term let compare = Terms.compare end)

    (* let iatom_build (a,d) = *)
    (*   let module M = LitB.Homo(IJMon) in *)
    (*   let get_ij iso =  *)
    (*     let k,_   = IntSort.reveal iso in *)
    (*     let fv,ar = Prop.DSubst.get k d in *)
    (*     (World.asIntSort fv, ar.World.next_eigen, ar.World.next_meta) *)
    (*   in *)
    (*   M.lift get_ij a, *)
    (*   M.lift get_ij (LitB.negation a) *)

    (* let makes_sense ((_,i,j),_) ar =  *)
    (*   (i <= ar.World.next_eigen) && (j >= ar.World.next_meta) *)

    include Unification.Make(Term)

  end

  open DS

  exception FoundIt of TSet.t*Constraint.t*(TSet.t,Constraint.t) stream

(* We range over atomN,
   alias is the set of atoms that haven't been tried (starts with atomN);
   as soon as we find a solution, we raise an exception *)

  let aux aset compare cont t atomN sigma = 
    TSet.fold
      (fun a alias -> 
        Dump.msg(Some(fun p -> p "Unifying literals %a and %a" Term.print_in_fmt a Term.print_in_fmt t))None None;
        let newalias = TSet.remove a alias in
        (match asL a, asL t with
        | (Some(b1,l1),_,_,_), (Some(b2,l2),_,_,_)
          when b1 = b2 ->
          (Dump.msg (Some(fun p -> p "constraint = %a" Constraint.print_in_fmt sigma))None None;
           Dump.msg(Some(fun p -> p "Actually unifying atoms %a and %a" Term.print_in_fmt l1 Term.print_in_fmt l2))None None;
           let internalise = IU.internalise (MKcorr.get_key sigma.Constraint.mk) in
           match Constraint.unif sigma [internalise l1] [internalise l2] with
           | Some newsigma -> 
             raise (FoundIt(TSet.add a aset,newsigma, cont newalias))
           | None -> ())
        | _ -> ());
        newalias)
      atomN 
      atomN 
      
  let rec goal_consistency t atomN sigma = 
  (* (print_endline (Dump.toString(fun p->p "goal consistency on %a |- %a" TSet.print_in_fmt atomN))); *)
  (* We range over atomN,
     we catch an exception (means success);
     otherwise we finish with NoMore *)
    try 
      let _ = 
        aux TSet.empty (=) (goal_consistency t) t atomN sigma
      in NoMore
    with
      FoundIt(a,newsigma,f) -> Guard(a,newsigma,f)

  let rec consistency atomN sigma =
    try 
      let _ =
        TSet.fold
          (fun t after -> 
            let newafter = TSet.remove t after in
            let aset = TSet.add t TSet.empty in
            let rec g_consistency remaining sigma =
              try 
                let _ = aux aset (!=) g_consistency t remaining sigma in
                consistency newafter sigma
              with
                FoundIt(a,newsigma,f) -> Guard(a,newsigma,f)
            in
            let _ = aux aset (!=) g_consistency t newafter sigma in newafter
          )
          atomN 
          atomN 
      in
      NoMore
    with
      FoundIt(a,newsigma,f) -> Guard(a,newsigma,f)

end

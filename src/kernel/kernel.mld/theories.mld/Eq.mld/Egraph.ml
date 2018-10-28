open General
open Sums
open Patricia_tools

open Top
open Terms
open Sassigns
open Values
open Messages

include Egraph_sig

(* The E-graph is a union-find structure where the nodes are either terms or values *)

(* Sum type for terms+values *)
module TermValue = struct
  type t = (Term.t,Value.t values) sum [@@deriving eq,ord,show,hash]
end

(* Valuations are term -> values maps extracted from the Egraph *)
module Valuation = struct
  (* We define a notion of valuation,
     as a map from terms to the combined values found in the egraph. *)
  module Arg = struct
    include Term
    include TypesFromHConsed(Term)
    include EmptyInfo
    type values = CValue.t * (Assign.t*int) Lazy.t
  end

  module Revealed = struct
    open Patricia
    include Map.MakeNH(Arg)
    let pp_pair fmt (term,(cval,_)) =
      Format.fprintf fmt "(%a↦ %a)" Term.pp term CValue.pp cval
    let pp = print_in_fmt ~wrap:("{","}") pp_pair
  end

  include Revealed

  let reveal t = t

end

(* Each E-graph component should know which other components they should never
   be equal to / merged with. We maintain for each component a set of disequalities 
   that are imposed on it. *)

module Diseq = struct
  include Patricia.Map.MakeNH(struct
      include BAssign
      include TypesFromHConsed(BAssign)
      include EmptyInfo
      type values = Term.t*Term.t
    end)
  let pp_binding fmt (j_neq,_) = BAssign.pp fmt j_neq
  let pp = print_in_fmt pp_binding
end


(* Sets of terms and values: those whose changes are listened to *)
module TVSet = struct
  include Set.Make(TermValue)
  let pp fmt tvset = List.pp TermValue.pp fmt (elements tvset)
end


(* Parameter module for UnionFind *)
module P = struct

  module Node = TermValue

  type edge = (BAssign.t,SAssign.t)sum [@@deriving show]

  (* The information we want to keep about each component *)
  type info = {
    nf  : Term.t;   (* Normal form *)
    cval: CValue.t; (* Combined value *)
    (* If a disequality j_neq, namely t1<>t2, was recorded with t1 in this component, the following Diseq contains a binding j_neq -> (t1,t2) *)
    diseq: Diseq.t;
    listening: TVSet.t
  } [@@deriving show] 

  let pp_binding keys_pp fmt (key,parent) =
    match parent with
    | Case1 parent -> Format.fprintf fmt "(%a → %a)" keys_pp key TermValue.pp parent
    | Case2 (id,size,info) -> Format.fprintf fmt "(%a → %i,%i,%a)"
                                keys_pp key id size pp_info info

  (* Following 2 modules: maps from terms to their parents in union find *)

  module TermMapArg = struct
    include Term
    include TypesFromHConsed(Term)
    include EmptyInfo
    type values = (Node.t, int * int * info) sum
  end

  module TermMap = struct
    include Patricia.Map.MakeNH(TermMapArg)
    let pp = print_in_fmt (pp_binding Term.pp)
  end

  (* Following 2 modules: maps from values to their parents in union find *)

  module ValueMapArg = struct
    type t = Value.t values [@@deriving eq,ord,show,hash]
  end

  module ValueMap = struct
    include Map.Make(ValueMapArg)
    let pp fmt m = List.pp (pp_binding ValueMapArg.pp) fmt (bindings m)
  end

  module NodeMapCompress = struct
    type t = TermMap.t * TermMap.values ValueMap.t
    let empty = TermMap.empty, ValueMap.empty
    let mem node (mterm,mval) = match node with
      | Case1 t -> TermMap.mem t mterm
      | Case2 v -> ValueMap.mem v mval
    let find node (mterm,mval) = match node with
      | Case1 t -> TermMap.find t mterm
      | Case2 v -> ValueMap.find v mval
    let add node parent (mterm,mval) = match node with
      | Case1 t -> TermMap.add t (fun _ -> parent) mterm, mval
      | Case2 v -> mterm, ValueMap.add v parent mval
  end

  module NodeMapProof = struct
    module TMP = Map.Make(TermValue)
    type t = (Node.t * edge) option TMP.t
    let empty = TMP.empty
    let mem = TMP.mem
    let find = TMP.find
    let add = TMP.add
  end

end

module UF = UnionFind.Make(P)

module Make(WTerm: Writable) = struct

    open P
    open UF
    type info = P.info
    type t = UF.t

    let init = init

    let singleton t = {
        nf        = t;
        cval      = t |> Terms.Term.get_sort |> CValue.none;
        diseq     = Diseq.empty;
        listening = TVSet.empty
      }

    let nf i = i.nf
    let cval i = i.cval
    let distinct (EGraph eg) i =
      let aux j (_,term) sofar =
        let _, pc = PC.get eg (Case1 term) in
        (PC.get_info pc).cval::sofar
      in
      Diseq.fold aux i.diseq []


    (* Generates equality inference *)
    let eq_inf j1 j2 =
      let SAssign(t1,v1) = SAssign.reveal j1 in
      let SAssign(t2,v2) = SAssign.reveal j2 in
      let justif = Assign.add j1 (Assign.singleton j2) in
      let eqterm = WTerm.bC (Symbols.Eq(Term.get_sort t1)) [t1;t2] in
      let eqassign = eqterm, Values.Boolean (Values.equal Value.equal v1 v2) in
      eqassign,
      straight () justif eqassign

    let pp_path = List.pp (pp_sum BAssign.pp SAssign.pp)
               
    (* Analyses a path from a term to a termvalue *)
    let treatpath path =
      let rec aux ?last propas assigns = function
        | []      -> last, propas, assigns
        | j::tail ->
           match j, last with
           | Case1 bassign, None ->
              aux propas (Assign.add (SAssign.build bassign) assigns) tail
           | Case1 _, Some _ ->
              failwith(Format.toString (fun p-> p "Path %a is ill-formed" pp_path path))
           | Case2 sassign, None ->
              aux ~last:sassign propas assigns tail
           | Case2 sassign1, Some sassign2 ->
              let j_eq, p = eq_inf sassign1 sassign2 in
              aux (p::propas) (Assign.add (SAssign.build j_eq) assigns) tail
      in
      aux [] Assign.empty path

           
    let eq   (* make two things equal in the E-graph *)
          t  (* a term *)
          t2 (* a term or value *)
          j  (* The single assignment justifying this merge *)
          (EGraph eg) (* the E-graph *)
      =
      let t1 = Case1 t in (* We turn t into an E-graph node *)
      (* We add t1 as its own E-graph component
         (will not change the E-graph if node exists) *)
      let EGraph eg = add t1 (singleton t) eg in 
      let EGraph eg = (* Same with t2 *)
        let info = match t2 with
          | Case1 t2' -> singleton t2'
          | Case2 v   -> {nf = t;
                          cval = CValue.inj v;
                          diseq = Diseq.empty;
                          listening = TVSet.empty }
        in
        add t2 info eg
      in
      (* We get the components of t1 and t2,
         and extract the compnent information for both *)
      let eg, pc1 = PC.get eg t1 in
      let eg, pc2 = PC.get eg t2 in
      let info1 = PC.get_info pc1 in
      let info2 = PC.get_info pc2 in
      (* Already in the same class? Do nothing. *)
      if PC.equal pc1 pc2
      then EGraph eg, info1, []
      else
        (* First: check whether the 2 components have not been declared different *)
        let merge_par = Diseq.Merge.{
            sameleaf = (fun bassign pair _ -> Some(bassign,pair));
            emptyfull= (fun _ -> None);
            fullempty= (fun _ -> None);
            combine  = (fun r1 r2 ->
              match r1, r2 with
              | Some _, _ -> r1
              | _, Some _ -> r2
              | None, None -> None)
          }
        in
        match Diseq.merge merge_par info1.diseq info2.diseq with
        | Some(j_neq,(t3,t4)) ->
           (* They were declared different by an assignment j_neq - a disequality
              between t3 and t4. Get the path from t1 to t3 and the one from t2 to t4. *)
           Print.print ["kernel.egraph",1] (fun p->
               p "kernel.egraph: violates %a" BAssign.pp j_neq);
           let path1 = path (Case1 t3) pc1 eg in
           let path2 = path (Case1 t4) pc2 eg in
           let path  = List.rev_append path1 (j::path2) in
           let _,propa,assign = treatpath path in
           let unsat_core = Assign.add (SAssign.build j_neq) assign in
           raise(Conflict(propa, unsat () unsat_core))
        | None ->
           (* They were not declared different. Check whether their values can be merged *)
           match CValue.merge info1.cval info2.cval with
           | Case1(v1,v2) ->
              (* Values couldn't be merged: they clash on v1 against v2.
                 v1 (resp. v2) must be in t1's component (resp t2's component).
                 We get the two paths. *)
              Print.print ["kernel.egraph",1] (fun p->
                  p "kernel.egraph: violation: get 2 values %a and %a"
                    (pp_values Value.pp) v1 (pp_values Value.pp) v2);
              let path1 = path (Case2 v1) pc1 eg in
              let path2 = path (Case2 v2) pc2 eg in
              let path = List.rev_append path1 (j::path2) in
              begin match path with
              | (Case2 j1)::path ->
                 let j2,propa,assign = treatpath path in
                 begin match j2 with
                 | Some j2 ->
                    let j_neq,p = eq_inf j1 j2 in
                    let unsat_core = Assign.add (SAssign.build j_neq) assign in
                    raise(Conflict(p::propa, unsat () unsat_core))
                 | _ -> failwith "Path does not finish on v2"
                 end
              | _ -> failwith "Path does not finish on v1"
              end
           | Case2 cval ->
              (* Values could be merged. Creating the info for the merged component. *)
              let nf = info1.nf in (* Normal form is that of t1 (Could be tuned!) *)
              (* The declared disequalities are the union of the two components. *)
              let diseq = Diseq.union (fun _ _ v -> v) info1.diseq info2.diseq in
              let listening = TVSet.union info1.listening info2.listening in
              let info = { nf; cval; diseq; listening } in
              (* We output the resulting egraph, the info of the merged component,
                 and the nodes that were listened to and have seen their values updated *)
              merge pc1 pc2 info j eg,
              info,
              let l1 = if CValue.equal cval info1.cval then TVSet.empty
                       else info1.listening in
              let l2 = if CValue.equal cval info2.cval then TVSet.empty
                       else info2.listening in
              let l = TVSet.fold (fun t sofar -> t::sofar) l1 [] in
              TVSet.fold (fun t sofar -> t::sofar) l2 l
                       
                       
    let diseq t1 t2 j (EGraph eg) =
      let tv1 = Case1 t1 in
      let tv2 = Case1 t2 in
      let EGraph eg = add tv1 (singleton t1) eg in
      let EGraph eg = add tv2 (singleton t2) eg in
      let eg,pc1 = PC.get eg tv1 in
      let eg,pc2 = PC.get eg tv2 in
      if PC.equal pc1 pc2 then
        let _,propas,assigns = treatpath(path tv1 pc2 eg) in
        let unsat_core = Assign.add (SAssign.build j) assigns in
        raise(Conflict(propas, unsat () unsat_core))
      else
        let info1 = PC.get_info pc1 in
        let EGraph eg =
          update pc1 {info1 with diseq = Diseq.add j (fun _ -> (t1,t2)) info1.diseq } eg
        in
        let eg,pc2 = PC.get eg tv2 in
        let info2 = PC.get_info pc2 in
        update pc2 {info2 with diseq = Diseq.add j (fun _ -> (t2,t1)) info2.diseq } eg

               
    let ask ?subscribe tv (EGraph eg) =
      let EGraph eg = match tv with
        | Case1 t -> add tv (singleton t) eg
        | Case2 v -> EGraph eg
      in
      let eg,pc = PC.get eg tv in
      let info = PC.get_info pc in
      match subscribe with
      | None -> info, EGraph eg
      | Some true ->
         let info = { info with listening = TVSet.add tv info.listening } in
         info, (update pc info eg)
      | Some false ->
         let info = { info with listening = TVSet.remove tv info.listening } in
         info, (update pc info eg)

    (* Picking n terms in a tset, or the maximum thereof *)
    let pick n =
      TSet.fold_monad
        ~return:(fun watchable -> watchable)
        ~bind:(fun reccall todo watchable ->
            if List.length watchable < n
            then reccall todo watchable
            else watchable)
        (fun var watchable -> var::watchable)

    let watchfind key n tset eg =
      let tm,_ = UF.extract eg in
      let action =
        (* Variable var is assigned in the model, can't pick it to watch *)
        let sameleaf var () parent (EGraph eg, fixed, watchable) =
          let eg,info = match parent with
            | Case1 tv ->
              let eg,pc = UF.PC.get eg tv in
              (EGraph eg : UF.t), UF.PC.get_info pc
            | Case2(_,_,info) ->
              (EGraph eg : UF.t),info
          in
          eg,
          let cv = cval info in
          match CValue.proj key cv with
          | None   -> { fixed;
                        unknown = TSet.singleton var;
                        watchable = var::watchable}
          | Some _ -> { fixed = Valuation.add
                            var (fun _ -> (cv,lazy (failwith "TODO"))) fixed;
                        unknown = TSet.empty;
                        watchable }
        in
        (* No variable in this part of the exploration *)
        let emptyfull _ (eg, fixed, watchable) =
          eg, { fixed; unknown = TSet.empty; watchable }
        in
        (* All vars in this part of the constraint are unassigned.
               we try to complete watchable to n terms: *)
        let fullempty tset (eg, fixed, watchable) =
          eg, { fixed; unknown = tset; watchable = pick n tset watchable }
        in

        (* Constraint is split in two, ans1 is the result from the left exploration.
                 (reccall rset rmodel) is the job to do for the right exploration. *)
        let combine ~reccall rset rmodel (eg,ans1) =
          if List.length ans1.watchable < n
          then
            let eg,ans2 = reccall rset rmodel (eg, ans1.fixed, ans1.watchable) in
            eg, { watchable = ans2.watchable;
                  fixed     = ans2.fixed;
                  unknown   = TSet.union ans1.unknown ans2.unknown }
          else
            eg, { ans1 with unknown = TSet.union ans1.unknown rset }
        in
        TermMap.Fold2.{ sameleaf; emptyfull; fullempty;
                        combine = make_combine
                            ~empty1:TSet.empty
                            ~empty2:TermMap.empty
                            combine }
      in
      TermMap.fold2_poly action tset tm (eg, Valuation.empty, [])

  end

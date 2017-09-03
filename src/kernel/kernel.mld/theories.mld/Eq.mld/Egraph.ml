open General
open Sums
open Patricia
open Patricia_tools

open Top
open Specs
open Sassigns
open Messages

open Interfaces
       
type sign = unit
              
module Make(DS: GlobalDS) = struct
  
  open DS

  type nonrec straight = (unit,straight) Msg.t
  type stop = straight list * (unit,unsat) Msg.t

  (* Sum type for terms+values *)

  module TermValue = struct
    type t = (Term.t,Value.t values) sum [@@deriving eq,ord,show,hash]                               
  end
                                           
  module TMap = struct
    include Map.Make(Term)
    let pp x fmt tmap =
      let ppb fmt (nf,j) = Format.fprintf fmt "(%a->%a)" Term.pp nf x j in
      List.pp ppb fmt (bindings tmap)
  end

  module BDest = struct
    type t = bassign [@@deriving ord]
    let id (t,Values.Boolean b) = 2*Term.id t + (if b then 0 else 1)
    type values = Term.t*Term.t
    include EmptyInfo
    let treeHCons = None
  end
                   
  module BMap = struct
    include PatMap.Make(BDest)(TypesFromHConsed(BDest))
    let pp_binding fmt (j_neq,_) = pp_bassign fmt j_neq
    let pp = print_in_fmt pp_binding
  end

  module TVSet = struct
    include Set.Make(TermValue)
    let pp fmt tvset = List.pp TermValue.pp fmt (elements tvset)
  end


  (* The information we want to keep about each component *)
  type info = {
      nf  : Term.t;   (* Normal form *)
      cval: CValue.t; (* Combined value *)
      (* If a disequality j_neq, namely t1<>t2, was recorded with t1 in this component, the following BMap contains a binding j_neq -> (t1,t2) *)
      diseq: BMap.t;
      listening: TVSet.t
    } [@@deriving eq,show] 
                
  module Make(REG : RawEgraph with type node = TermValue.t
                               and type edge = (bassign,sassign)sum
                               and type info = info)
    = struct

    open REG
    type t = REG.t

    let init = init
                 
    exception Conflict of stop

    let singleton t = {
        nf        = t;
        cval      = CValue.none (Term.get_sort t);
        diseq     = BMap.empty;
        listening = TVSet.empty
      }

    let nf i = i.nf
    let cval i = i.cval
    let distinct (EGraph eg) i =
      let aux j (_,term) sofar =
        let _, pc = PC.get eg (Case1 term) in
        (PC.get_info pc).cval::sofar
      in
      BMap.fold aux i.diseq []


    (* Generates equality inference *)
    let eq_inf j1 j2 =
      let SAssign(t1,v1) = j1 in
      let SAssign(t2,v2) = j2 in
      let justif = Assign.add j1 (Assign.singleton j2) in
      let eqterm = Term.bC (Symbols.Eq(Term.get_sort t1)) [t1;t2] in
      let eqassign = eqterm, Values.Boolean (Values.equal Value.equal v1 v2) in
      eqassign,
      straight () justif eqassign

    let pp_path = List.pp (pp_sum pp_bassign pp_sassign)
               
    (* Analyses a path from a term to a termvalue *)
    let treatpath path =
      let rec aux ?last propas assigns = function
        | []      -> last, propas, assigns
        | j::tail ->
           match j, last with
           | Case1 bassign, None ->
              aux propas (Assign.add (SAssign bassign) assigns) tail
           | Case1 _, Some _ ->
              failwith(Print.toString (fun p-> p "Path %a is ill-formed" pp_path path))
           | Case2 sassign, None ->
              aux ~last:sassign propas assigns tail
           | Case2 sassign1, Some sassign2 ->
              let j_eq, p = eq_inf sassign1 sassign2 in
              aux (p::propas) (Assign.add (SAssign j_eq) assigns) tail
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
                          diseq = BMap.empty;
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
        let merge_par = BMap.Merge.{
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
        match BMap.merge merge_par info1.diseq info2.diseq with
        | Some(j_neq,(t3,t4)) ->
           (* They were declared different by an assignment j_neq - a disequality
              between t3 and t4. Get the path from t1 to t3 and the one from t2 to t4. *)
           Print.print ["kernel.egraph",1] (fun p->
               p "kernel.egraph: violates %a" pp_bassign j_neq);
           let path1 = path (Case1 t3) pc1 eg in
           let path2 = path (Case1 t4) pc2 eg in
           let path  = List.rev_append path1 (j::path2) in
           let _,propa,assign = treatpath path in
           let unsat_core = Assign.add (SAssign j_neq) assign in
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
                    let unsat_core = Assign.add (SAssign j_neq) assign in
                    raise(Conflict(p::propa, unsat () unsat_core))
                 | _ -> failwith "Path does not finish on v2"
                 end
              | _ -> failwith "Path does not finish on v1"
              end
           | Case2 cval ->
              (* Values could be merged. Creating the info for the merged component. *)
              let nf = info1.nf in (* Normal form is that of t1 (Could be tuned!) *)
              (* The declared disequalities are the union of the two components. *)
              let diseq = BMap.union (fun _ v -> v) info1.diseq info2.diseq in
              let listening = TVSet.union info1.listening info2.listening in
              let info = { nf=nf; cval=cval; diseq=diseq; listening = listening } in
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
        let unsat_core = Assign.add (SAssign j) assigns in
        raise(Conflict(propas, unsat () unsat_core))
      else
        let info1 = PC.get_info pc1 in
        let EGraph eg =
          update pc1 {info1 with diseq = BMap.add j (fun _ -> (t1,t2)) info1.diseq } eg
        in
        let eg,pc2 = PC.get eg tv2 in
        let info2 = PC.get_info pc2 in
        update pc2 {info2 with diseq = BMap.add j (fun _ -> (t2,t1)) info2.diseq } eg

               
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

  end
end

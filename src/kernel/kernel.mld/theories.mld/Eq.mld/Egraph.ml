open General
open Sums
open SetConstructions
       
open Patricia
open Top
open Specs
open Messages

open Interfaces
       
type sign = unit
              
module Make(DS: GlobalDS) = struct
  
  open DS

  type straight = (unit,Assign.t*bassign,Messages.straight) message
  type stop     = straight list * ((sign,Assign.t*bassign,unsat) message)

  (* Sum type for terms+values *)

  module TermValue = struct
    type t = (Term.t,Value.t Values.t) sum
               [@@deriving eq,ord,show,hash]                               
  end
                                           
  module TMap = struct
    include Map.Make(Term)
    let pp x fmt tmap =
      let aux fmt (nf,j) = Format.fprintf fmt "(%a->%a)" Term.pp nf x j in
      List.pp aux fmt (bindings tmap)
  end

  module BDest = struct
    type keys = bassign
    let kcompare = compare_bassign
    type values = Term.t*Term.t
    type infos = unit
    let info_build = empty_info_build
    let treeHCons = None
  end
                   
  module TCons = TypesFromHConsed(struct
                     type t = bassign
                     let id (t,_) = Term.id t
                   end)
  module BCons = TypesFromHConsed(struct
                     type t = bassign
                     let id (_,b) = if b then 0 else 1
                   end)
  module I = LexProduct(TCons)(BCons)
  module BMap = struct
    include PATMap.Make(BDest)(I)
    let pp fmt bmap =
      let ppb fmt (bassign,term) =
        Format.fprintf fmt "(%a->%a)" pp_bassign bassign Term.pp term
      in
      print_in_fmt ppb
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
    }
                [@@deriving eq,show] 
                
  module Make(REG : RawEgraph with type node := TermValue.t
                                            and type edge := sassign
                                                         and type info := info)
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
      let t1,v1 = j1 in
      let t2,v2 = j2 in
      let justif = Assign.add j1 (Assign.singleton j2) in
      let eqterm = Term.bC (Symbols.Eq(Term.get_sort t1)) [t1;t2] in
      let eqassign = eqterm,(Values.equal Value.equal v1 v2) in
      Values.boolassign eqassign,
      straight () justif eqassign
               
    (* Analyses a path from a term to a termvalue *)
    let treatpath path =
      let rec aux ?last propas assigns = function
        | []      -> last, propas, assigns
        | j::tail ->
           match j, last with
           | (_,Values.Boolean _), None -> aux propas (Assign.add j assigns) tail
           | (_,Values.Boolean _), Some _
             -> failwith(Dump.toString (fun p->
                             p "Path %a is ill-formed" (List.pp pp_sassign) path))
           | _, None
             -> aux ~last:j propas assigns tail
           | (t, _), Some((t',_) as j') ->
              let j_eq, p = eq_inf j j' in
              aux (p::propas) (Assign.add j_eq assigns) path
      in
      aux [] Assign.empty path

          
    let explain j  (* single assignment in question t1->v or t1=t2. *)
          j2       (* Some(t2->v) or None *)
          propas   (* equality inferences to propagate before conflict *)
          assign1  (* assignments from t1 to t3 *)
          assign2  (* assignments from t2 to t4 *)
          j_neq    (* assignment that t3 and t4 are different *)
      =
      let propas, j_eq =
        match j2 with
        | None    -> propas, j
        | Some j2 -> let j_eq, p = eq_inf j j2 in
                     (p::propas), j_eq
      in
      let res = Assign.union assign1 assign2 in
      let unsat_core = Assign.add j_neq (Assign.add j_eq res) in
      raise(Conflict(propas, unsat () unsat_core))

           
    let eq t t2 j (EGraph eg) =
      let t1 = Case1 t in
      let EGraph eg = add t1 (singleton t) eg in
      let EGraph eg =
        let info = match t2 with
          | Case1 t2' -> singleton t2'
          | Case2 v   -> {nf = t;
                          cval = CValue.inj v;
                          diseq = BMap.empty;
                          listening = TVSet.empty }
        in
        add t2 info eg
      in
      let eg, pc1 = PC.get eg t1 in
      let eg, pc2 = PC.get eg t2 in
      let info1 = PC.get_info pc1 in
      let info2 = PC.get_info pc2 in
      if PC.equal pc1 pc2
      then merge pc1 pc2 info1 j eg, info1, []
      else
        let merge_par = let open BMap in {
            sameleaf = (fun bassign t1 _ -> Some(bassign,t1));
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
           let _,propa1,assign1 = treatpath(path (Case1 t3) pc1 eg) in
           let j_last,propa2,assign2 = treatpath(path (Case1 t4) pc2 eg) in
           let propas = List.append propa1 propa2 in
           explain j j_last propas assign1 assign2 (Values.boolassign j_neq)
        | None ->
           match CValue.merge info1.cval info2.cval with
           | Case1(v1,v2) ->
              let path1 = path (Case2 v1) pc1 eg in
              let path2 = path (Case2 v2) pc2 eg in
              begin
                match path1, path2 with
                | j1::path1, j2::path2
                  -> let _,propa1,assign1 = treatpath path1 in
                     let j_last,propa2,assign2 = treatpath path2 in
                     let j_neq,p = eq_inf j1 j2 in
                     let propas = p::(List.append propa1 propa2) in
                     explain j j_last propas assign1 assign2 j_neq
                | _ -> failwith "Paths to values are empty"
              end
           | Case2 cval ->
              let nf = info1.nf in
              let diseq = BMap.union (fun _ v -> v) info1.diseq info2.diseq in
              let listening = TVSet.union info1.listening info2.listening in
              let info = { nf=nf; cval=cval; diseq=diseq; listening = listening } in
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
        let unsat_core = Assign.add (Values.boolassign j) assigns in
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

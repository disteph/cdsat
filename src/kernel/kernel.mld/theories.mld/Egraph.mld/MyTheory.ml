open General.Sums

open Top
open Specs
open Interfaces_basic
open Messages
       
type sign = unit

(* The EGraph doesn't add its own kind of values, it uses the other theories' *)
include Theory.HasNoValues

(* The EGraph does not need alterative term representations *)
type ts = unit
let ts = Termstructures.Register.NoRepModule

(* API for plugin. So far, let's pretend it's empty *)

module type API = sig

end

                    
module Make(DS: DSproj with type ts = Clauses.TS.t) = struct
  
  open DS

  type term   = Term.t
  type value  = Value.t
  type assign = Assign.t

  type boolassign = Term.t*bool [@@deriving eq,show]
  type straight = (unit,Assign.t*boolassign,Messages.straight) message
  type stop = straight list * ((sign,Assign.t*boolassign,unsat) message)

  (* Sum type for terms+values *)

  module TermValue = struct
    type t = (Term.t,Value.t Values.t) sum
               [@@deriving eq,ord,show,hash]                               
  end

  (* Abbreviation for single assignments *)
  type sassign = Term.t*(Value.t Values.t) [@@deriving eq,show]

  module TMap = struct
    include Map.Make(Term)
    let pp x fmt tmap =
      let aux fmt (nf,j) = Format.fprintf fmt "(%a->%a)" Term.pp nf x j in
      List.pp aux fmt (bindings tmap)
  end

  module TVMap = Map.Make(TermValue)

  module TVSet = struct
    include Set.Make(TermValue)
    let pp fmt tvset = List.pp TermValue.pp fmt (elements tvset)
  end


  (* The information we want to keep about each component *)
  type info = {
      nf  : Term.t; (* Normal form *)
      cval: CValue.t; (* Combined value *)
      (* Terms that are declared disequal from this component have an entry,
         mapped to the corresponding single assignment *)
      diseq: (Term.t*Term.t*boolassign) TMap.t;
      listening: TVSet.t
    }
                [@@deriving eq,show]
                
  (* The is the interface for the raw implementation of the egraph, i.e. the union-find structure.
     A pointed component is an EGraph component of a specific node;
     it is pointed because it remembers which node it is the component of. *)
                
  module type PointedComp = sig
    type _ egraph
    type e_graph = EGraph : _ egraph -> e_graph
    type _ t

    val init  : e_graph
           
    val equal : 'a t -> 'a t -> bool
    (* Getting the pointed component of a node.
       Fails if node does not exist. *)
    val get   : 'a egraph -> TermValue.t -> 'a t
    (* Get info of pointed component *)
    val info  : _ t -> info
    (* Add new singleton component.
       If it exists, returns the original graph. *)
    val add   : TermValue.t -> info -> _ egraph -> e_graph
    (* Update info of pointed component *)
    val update : 'a t -> info -> 'a egraph -> e_graph
    (* Merge 2 pointed components, adding edge between the 2 points,
       using info for merged component *)
    val merge : 'a t -> 'a t -> info -> sassign -> 'a egraph -> e_graph
    (* Provide path between 2 nodes of the same component *)
    val path  : TermValue.t -> TermValue.t -> _ egraph -> sassign list
  end

  module PointedComp : PointedComp = struct

    type _ egraph = {
        (* Number of components *)
        count  : int;
        (* Next element of component.
           Representative is mapped to the component information, to which we add:
           the id of the component and its size *)
        next   : (TermValue.t,int*int*info) sum TVMap.t;
        (* All E-graph edges, bidirectional. Used to compute explanations. *)
        others : sassign TVMap.t TVMap.t;
      }

    type e_graph = EGraph : _ egraph -> e_graph (* [@@unboxed] *)

    let init = EGraph { count = 0; next = TVMap.empty; others = TVMap.empty }
                                          
    type _ t = {
        pointed : TermValue.t; (* Node of which we have fetched the component *)
        id   : int;  (* id of component *)
        size : int;  (* size of component *)
        info : info; (* information of component *)
        representative : TermValue.t; (* Current representative of component *)
        intermediates  : TermValue.t list; (* Intermediate nodes seen from pointed to representative *)
      }

                 
    let equal : 'a t -> 'a t -> bool
      = fun t t' -> (t.id = t'.id)

    let get : 'a egraph -> TermValue.t -> 'a t =
      fun eg tv ->
      let rec aux tv' intermediates =
        match TVMap.find tv' eg.next with
        | Case1 tv'' -> aux tv'' (tv'::intermediates)
        | Case2(id,size,info) -> {
            pointed = tv;
            id   = id;
            size = size;
            info = info;
            representative = tv';
            intermediates = intermediates;
          }
      in aux tv []

    let add : TermValue.t -> info -> _ egraph -> e_graph
      = fun tv info eg ->
      if TVMap.mem tv eg.next
      then EGraph eg
      else
        EGraph
          { eg with
            count = eg.count+1;
            next  = TVMap.add
                      tv
                      (Case2(eg.count+1,1,info))
                      eg.next }
          
    let info x = x.info

    let update : 'a t -> info -> 'a egraph -> e_graph
      = fun pc info eg ->
      EGraph
        { eg with next = TVMap.add
                           pc.representative
                           (Case2(pc.id,pc.size,info))
                           eg.next }
        
    let tvmap_add t1 t2 j map =
      let prev =
        if TVMap.mem t1 map
        then TVMap.find t1 map
        else TVMap.empty
      in
      TVMap.add t1 (TVMap.add t2 j prev) map

    let merge : 'a t -> 'a t -> info -> sassign -> 'a egraph -> e_graph
      = fun pc1 pc2 info j eg ->
      if equal pc1 pc2
      then
        update pc1 info eg
      else
        let eg =
          if TermValue.equal pc1.pointed pc2.pointed
          then eg
          else { eg with
                 others = tvmap_add pc1.pointed pc2.pointed j
                            (tvmap_add pc2.pointed pc1.pointed j eg.others)
               }
        in
        let rec aux next pckeep tv = function
          | [] ->
             let newsize = pc1.size + pc2.size in
             let next = TVMap.add pckeep.representative (Case2(pckeep.id,newsize,info)) next in
             let next = TVMap.add tv (Case1 pckeep.pointed) next in
             EGraph { eg with count = eg.count-1; next = next }
          | tv'::intermediates ->
             let next = TVMap.add tv (Case1 tv') next in
             aux next pckeep tv' intermediates
        in
        let pckeep,tv,intermediates =
          if List.length pc1.intermediates < List.length pc2.intermediates
          then pc1,pc2.pointed,pc2.intermediates
          else pc2,pc1.pointed,pc1.intermediates
        in
        aux eg.next pckeep tv intermediates
                                 
    let path t1 t2 = failwith "TODO"

  end

                     
  module type EGraph = sig
    exception Conflict of stop
    type t
    val init : t
    val eq : Term.t -> TermValue.t -> sassign -> t -> t*info*TVSet.t
    val diseq : Term.t -> Term.t -> boolassign -> t -> t
    (* Ask information about the termvalue,
       possibly subscribe (subscribe=true) or unsubscribe (subscribe=false)
       to notifications when the termvalue sees its combined value affected *)
    val ask : ?subscribe:bool -> TermValue.t -> t -> info*t
  end

  module EGraph(PC:PointedComp) : EGraph = struct

    open PC
    type t = e_graph

    let init = PC.init
                 
    exception Conflict of stop

    let singleton t = {
        nf        = t;
        cval      = CValue.none (Term.get_sort t);
        diseq     = TMap.empty;
        listening = TVSet.empty
      }

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
                          diseq = TMap.empty;
                          listening = TVSet.empty }
        in
        add t2 info eg
      in
      let pc1 = get eg t1 in
      let pc2 = get eg t2 in
      let info1 = info pc1 in
      let info2 = info pc2 in
      if equal pc1 pc2
      then merge pc1 pc2 info1 j eg, info1, TVSet.empty
      else
        let aux k ((t3,t4,j_neq) as neq) sofar =
          let pc = get eg (Case1 k) in
          if equal pc pc2
          then
            let _,propa1,assign1 = treatpath(path (Case1 t3) t1 eg) in
            let j_last,propa2,assign2 = treatpath(path (Case1 t4) t2 eg) in
            let propas = List.append propa1 propa2 in
            explain j j_last propas assign1 assign2 (Values.boolassign j_neq)
          else TMap.add (info pc).nf neq sofar
        in
        let diseq = TMap.fold aux info1.diseq TMap.empty in
        match CValue.merge info1.cval info2.cval with
        | Case1(v1,v2) ->
           let path1 = path (Case2 v1) t1 eg in
           let path2 = path (Case2 v2) t2 eg in
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
           let diseq = TMap.union (fun _ j _ -> Some j) diseq info2.diseq in
           let listening = TVSet.union info1.listening info2.listening in
           let info = { nf=nf; cval=cval; diseq=diseq; listening = listening } in
           merge pc1 pc2 info j eg,
           info,
           let l1 = if CValue.equal cval info1.cval then TVSet.empty
                    else info1.listening in
           let l2 = if CValue.equal cval info2.cval then TVSet.empty
                    else info2.listening in
           TVSet.union l1 l2
           
                 
    let diseq t1 t2 j (EGraph eg) =
      let tv1 = Case1 t1 in
      let tv2 = Case1 t2 in
      let EGraph eg = add tv1 (singleton t1) eg in
      let EGraph eg = add tv2 (singleton t2) eg in
      let pc1 = get eg tv1 in
      let pc2 = get eg tv2 in
      if equal pc1 pc2 then
        let _,propas,assigns = treatpath(PC.path tv1 tv2 eg) in
        let unsat_core = Assign.add (Values.boolassign j) assigns in
        raise(Conflict(propas, unsat () unsat_core))
      else
        let info1 = info pc1 in
        let info2 = info pc2 in
        let EGraph eg =
          update pc1 {info1 with diseq = TMap.add info2.nf (t1,t2,j) info1.diseq } eg
        in
        let pc2 = get eg tv2 in
        let info2 = info pc2 in
        update pc2 {info2 with diseq = TMap.add info1.nf (t2,t1,j) info2.diseq } eg

    let ask ?subscribe tv (EGraph eg) =
      let EGraph eg = match tv with
        | Case1 t -> add tv (singleton t) eg
        | Case2 v -> EGraph eg
      in
      let pc = get eg tv in
      let info = PC.info pc in
      match subscribe with
      | None -> info, EGraph eg
      | Some true ->
         let info = { info with listening = TVSet.add tv info.listening } in
         info, (update pc info eg)
      | Some false ->
         let info = { info with listening = TVSet.remove tv info.listening } in
         info, (update pc info eg)

  end

  module EG = EGraph(PointedComp)
                    
  type state = {
      egraph : EG.t;
      treated: Assign.t
    }

  let add state sassign =
    Dump.print ["egraph",1] (fun p-> p "EGraph adds %a" pp_sassign sassign);
    let term,value = sassign in
    let treated = Assign.add sassign state.treated in
    try
      let eg,info,tvset = EG.eq term (Case2 value) sassign state.egraph in
      let tvmap = TVSet.fold (fun x -> TVMap.add x info) tvset TVMap.empty in
      let eg, tvmap =
        match Terms.reveal term, value with
        | Terms.C(Symbols.Eq s,[t1;t2]), Values.Boolean true
          | Terms.C(Symbols.NEq s,[t1;t2]), Values.Boolean false
          -> let eg,info,tvset = EG.eq t1 (Case1 t2) sassign eg in
             let tvmap = TVSet.fold (fun x -> TVMap.add x info) tvset tvmap in
             eg, tvmap
        | Terms.C(Symbols.NEq s,[t1;t2]), Values.Boolean true
          -> EG.diseq t1 t2 (term,true) eg, tvmap
        | Terms.C(Symbols.Eq s,[t1;t2]), Values.Boolean false
          -> EG.diseq t1 t2 (term,false) eg, tvmap
        | _ -> eg, tvmap
      in
      Dump.print ["egraph",1] (fun p-> p "EGraph is fine with %a" Assign.pp treated);
      (* Output(Some(sat () atomN), machine assign) *)
      { egraph = eg ; treated = treated }
    with
      EG.Conflict _ -> failwith "TODO"
      (* Output(Some(unsat () tset), Tools.fail_state) *)

  let clone ()  = failwith "TODO" (* Output(None, machine assign) *)
  let suicide _ = ()

  let init = EG.init
  let clear () = ()
                          
end

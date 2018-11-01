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

(* Each E-graph component should know which other components they should never
   be equal to / merged with. We maintain for each component a set of disequalities 
   that are imposed on it. *)

module Diseq = struct
  include Patricia.Map.MakeNH(struct
      include SAssign
      include TypesFromHConsed(SAssign)
      include EmptyInfo
      type values = int*Term.t*Term.t
    end)
  let pp_binding fmt (j_neq,(i,_,_)) =
    Format.fprintf fmt "%a since level %i" SAssign.pp j_neq i
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

  type edge = {sassign   : SAssign.t;
               level     : int } [@@deriving show]

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
    type info = P.info
    include UF

    let singleton t = {
        nf        = t;
        cval      = t |> Terms.Term.get_sort |> CValue.none;
        diseq     = Diseq.empty;
        listening = TVSet.empty
      }

    let nf i = i.nf
    let cval i = i.cval
    let distinct i =
      let aux j (_,_,term) sofar =
        let%bind sofar = sofar in
        let%map info   = get_info (Case1 term) in
        info.cval::sofar
      in
      Diseq.fold aux i.diseq (EMonad.return [])


    (* Generates equality inference *)
    let eq_inf edge1 edge2 =
      let SAssign(t1,v1) = SAssign.reveal edge1.sassign in
      let SAssign(t2,v2) = SAssign.reveal edge2.sassign in
      let justif = Assign.add edge1.sassign (Assign.singleton edge2.sassign) in
      let level  = max edge1.level edge2.level in
      let eqterm = WTerm.bC (Symbols.Eq(Term.get_sort t1)) [t1;t2] in
      let eqassign = eqterm, Values.Boolean (Values.equal Value.equal v1 v2) in
      eqassign, straight () justif eqassign, level

    let trans_inf justif t1 t2 =
      let eqterm = WTerm.bC (Symbols.Eq(Term.get_sort t1)) [t1;t2] in
      eqterm, Values.Boolean true
      
    let rec path_level = function
      | []            -> failwith "Paths should have at least one edge"
      | [{level},t,_] -> level
      | ({level},t,_)::path -> max level (path_level path)

    let path_fst_term = function
      | (_,Case1 t,_)::_ -> t
      | (_,Case2 _,_)::_
      | []         -> failwith "Paths should have at least one edge"
      
    (* Analyses a path from a term to a termvalue *)
    let treatpath path =
      (* Global level of the path *)
      let pathlevel = path_level path in
      (* In the next function aux:
         assigns and current work together,
         representing the compressed path from the original origin (a term)
         to the last term that has been seen.
         The compressed path is made of segments
         (each segment is made of edges of level < pathlevel,
         or of level exactly pathlevel).
         Finished segments are in assigns in the form of equalities
         (between the term starting the segment and the term finishing the segment).
         The current segment is in current, as a 4-tuple
         (assign, term starting the segment, last term to be seen, level of segment).
         last: present if the last node we have seen is a value
         path: the rest of the path to treat
         propas: propagations justifying the path compressions so far, with their levels.
      *)
      let segment_close current =
        let assign, t1, t1', level = current in
        if Assign.is_empty assign
        then None
        else
          let eqassign = trans_inf assign t1 t1' in
          Some(SAssign.build eqassign, (straight () assign eqassign,level))
      in

      let rec aux assigns current ?last path propas =
        match last, path with
        | _,[]      ->
          begin
            (* We close the current segment *)
            match segment_close current with
            | None                -> last, propas, assigns, pathlevel
            | Some(sassign,propa) -> last, propa::propas, Assign.add sassign assigns, pathlevel
          end

        | None, (edge, Case1 t, Case2 v)::path ->
          (* The last node we saw was a term (must be t) & its edge now goes to a value. 
             We load it in "last". *)
          aux assigns current ~last:(edge, t, v) path propas

        | None, (edge, Case1 t, Case1 t')::path ->
          (* The last node we saw was a term (must be t) & its edge now goes to a term. 
             We test whether it should be in the same segment as current. *)
          let assign, t1, t1', level = current in
          assert(Term.equal t1' t);
          if [%eq:bool] (edge.level = pathlevel) (level = pathlevel)
          then (* Same segment: we add the edge to the current segment *)
            let current = Assign.add edge.sassign assign, t1, t', max level edge.level in
            aux assigns current path propas
          else (* Different segment: we close the current segment and start a new one. *)
            let newcurrent = Assign.singleton edge.sassign, t, t', edge.level in
            begin (* We close the current segment *)
              match segment_close current with
              | None                ->
                aux assigns newcurrent path propas
              | Some(sassign,propa) ->
                aux (Assign.add sassign assigns) newcurrent path (propa::propas)
            end

        | Some(edge1, t1, v1), (edge2, Case2 v2, (Case1 t2 as tv2))::path ->
          (* assert (Values.equal Value.equal v1 v2); *)
          let eqassign, propa, level = eq_inf edge1 edge2 in
          let edge = { sassign = SAssign.build eqassign; level } in
          aux assigns current ((edge,Case1 t1,tv2)::path) propas

        | _, (_, Case2 _, Case2 _)::_ (* We shouldn't have an edge between 2 values *)
        | Some _, (_, Case1 _, _)::_ (* Edge2value must be followed by edge from value *)
        | None, (_, Case2 _, _)::_   (* Edge from value must be preceeded by edge2value *)
          -> failwith(Format.toString (fun p-> p "Path %a is ill-formed" pp_path path))

      in
      let fst_term = path_fst_term path in
      aux Assign.empty (Assign.empty, fst_term, fst_term, pathlevel) path [] 


    let eq   (* make two things equal in the E-graph *)
          t  (* a term *)
          t2 (* a term or value *)
          sassign (* The single assignment justifying this merge *)
          ~level  (* The level of this assignment *)
      =
      let t1 = Case1 t in (* We turn t into an E-graph node *)
      let newedge = { sassign; level } in
      (* We add t1 as its own E-graph component
         (will not change the E-graph if node exists) *)
      add t1 (singleton t);%bind
      let info = match t2 with
        | Case1 t2' -> singleton t2'
        | Case2 v   -> {nf = t;
                        cval = CValue.inj v;
                        diseq = Diseq.empty;
                        listening = TVSet.empty }
      in
      add t2 info;%bind
      let%bind info1 = get_info t1 in
      let%bind info2 = get_info t2 in
      (* Already in the same class? Do nothing. *)
      if%bind are_connected t1 t2
      then EMonad.return (info1, [])
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
        | Some(j_neq,(level,t3,t4)) ->
           (* They were declared different by an assignment j_neq - a disequality
              between t3 and t4. Get the path from t1 to t3 and the one from t2 to t4. *)
           Print.print ["kernel.egraph",1] (fun p->
               p "kernel.egraph: violates %a" SAssign.pp j_neq);
           let%bind path1 = get_path t1 (Case1 t3) in (* path from t1 to t3*)
           let%bind path2 = get_path t2 (Case1 t4) in (* path from t2 to t4*)
           let path = path_rev_append path1 ((newedge,t1,t2)::path2) in (* path from t3 to t4 *)
           let _,propa,assign,pathlevel = treatpath path in
           let unsat_core = Assign.add j_neq assign in
           raise(Conflict(propa, (unsat () unsat_core, max pathlevel level)))
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
              let%bind path1 = get_path t1 (Case2 v1) in (* path from t1 to v1*)
              let%bind path2 = get_path t2 (Case2 v2) in (* path from t2 to v2*)
              let path = List.rev_append path1 ((newedge,t1,t2)::path2) in (* path from v1 to v2 *)
              begin match path with
              | (edge1,Case2 vv1,Case1 tt1)::path when equal_values Value.equal vv1 v1 ->
                 let j2,propa,assign,pathlevel = treatpath path in
                 begin match j2 with
                 | Some(edge2, tt2, vv2) when equal_values Value.equal vv2 v2  ->
                    let j_neq,p,level = eq_inf edge1 edge2 in
                    let unsat_core = Assign.add (SAssign.build j_neq) assign in
                    raise(Conflict((p,max edge1.level edge2.level)::propa,
                                   (unsat () unsat_core,
                                    max pathlevel level)))
                 | _ -> failwith "Path does not finish with v2"
                 end
              | _ -> failwith "Path does not start with v1"
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
              merge t1 t2 info newedge;%map
              info,
              let l1 = if CValue.equal cval info1.cval then TVSet.empty
                       else info1.listening in
              let l2 = if CValue.equal cval info2.cval then TVSet.empty
                       else info2.listening in
              let l = TVSet.fold (fun t sofar -> t::sofar) l1 [] in
              TVSet.fold (fun t sofar -> t::sofar) l2 l
                       

    let diseq t1 t2 j ~level =
      let tv1 = Case1 t1 in
      let tv2 = Case1 t2 in
      add tv1 (singleton t1);%bind
      add tv2 (singleton t2);%bind
      if%bind are_connected tv1 tv2 then
        let%bind path = get_path tv1 tv2 in
        let last,propas,assigns,pathlevel = treatpath path in
        match last with
        | Some _ -> failwith "Should be None"
        | None ->
          let unsat_core = Assign.add j assigns in
          raise(Conflict(propas,
                         (unsat () unsat_core,
                          max pathlevel level)))
      else
        let%bind info1 = get_info tv1 in
        let diseq = Diseq.add j (fun _ -> (level,t1,t2)) info1.diseq in
        update tv1 {info1 with diseq };%bind
        let%bind info2 = get_info tv2 in
        let diseq = Diseq.add j (fun _ -> (level,t2,t1)) info2.diseq in
        update tv2 {info2 with diseq };%map
        ()
               
    let ask ?subscribe tv =
      begin match tv with
        | Case1 t -> add tv (singleton t)
        | Case2 v -> EMonad.return ()
      end;%bind
      let%bind info = get_info tv in
      match subscribe with
      | None -> EMonad.return info
      | Some true ->
        let info = { info with listening = TVSet.add tv info.listening } in
        update tv info;%map
        info
      | Some false ->
        let info = { info with listening = TVSet.remove tv info.listening } in
        update tv info;%map
        info

    (* Picking n terms in a tset, or the maximum thereof *)
    let pick n =
      TSet.fold_monad
        ~return:(fun watchable -> watchable)
        ~bind:(fun reccall todo watchable ->
            if List.length watchable < n
            then reccall todo watchable
            else watchable)
        (fun var watchable -> var::watchable)

    let watchfind key ~howmany tset =
      let%bind (tm,_) = extract in
      let action =
        (* Variable var is assigned in the model, can't pick it to watch *)
        let sameleaf var () parent (fixed, watchable) =
          let%bind info = match parent with
            | Case1 tv        -> get_info tv
            | Case2(_,_,info) -> EMonad.return info
          in
          let cv = cval info in
          match CValue.proj key cv with
          | None   -> EMonad.return { fixed;
                                      unknown = TSet.singleton var;
                                      watchable = var::watchable}
          | Some v ->
            let%map assign = elazy
              begin
                let%map path = get_path (Case1 var) (Case2(Values.inj key v)) in
                let aux ({sassign;level},_,_) (assign,levelsofar) =
                  Assign.add sassign assign, max level levelsofar
                in
                List.fold aux path (Assign.empty,-1)
              end
            in
            let add_aux = function
              | Some a -> a
              | None   -> cv, assign
                
            in
            { fixed   = Valuation.add var add_aux fixed;
              unknown = TSet.empty;
              watchable }
        in
        (* No variable in this part of the exploration *)
        let emptyfull _ (fixed, watchable) =
          EMonad.return { fixed; unknown = TSet.empty; watchable }
        in
        (* All vars in this part of the constraint are unassigned.
           we try to complete watchable to n terms: *)
        let fullempty tset (fixed, watchable) =
          EMonad.return { fixed; unknown = tset; watchable = pick howmany tset watchable }
        in

        (* Constraint is split in two, ans1 is the result from the left exploration.
           (reccall rset rmodel) is the job to do for the right exploration. *)
        let combine ~reccall rset rmodel ans1 =
          if List.length ans1.watchable < howmany
          then
            let%map ans2 = reccall rset rmodel (ans1.fixed, ans1.watchable) in
            { watchable = ans2.watchable;
              fixed     = ans2.fixed;
              unknown   = TSet.union ans1.unknown ans2.unknown }
          else
            EMonad.return { ans1 with unknown = TSet.union ans1.unknown rset }
        in
        let combine ~reccall rset rmodel = EMonad.bind (combine ~reccall rset rmodel) in
        TermMap.Fold2.{ sameleaf; emptyfull; fullempty;
                        combine = make_combine
                            ~empty1:TSet.empty
                            ~empty2:TermMap.empty
                            combine }
      in
      let%map result = TermMap.fold2_poly action tset tm (Valuation.empty, []) in
      { result with fixed = Valuation.build () result.fixed }

  end

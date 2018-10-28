open Format
open General
open Sums

include UnionFind_sig

module Make(P : Parameters) = struct

  open P

  type _ egraph = {
      (* Number of components *)
      count : int;
      (* First available component id *)
      available : int;
      (* Parent element in e-graph, compressing paths.
         Root representative is mapped to the component information, to which we add:
         the id of the component and its size *)
      parentWcomp : NodeMapCompress.t;
      (* Parent element, without path compression. Root is mapped to None.
         Used to produce explanations. *)
      parent : NodeMapProof.t;
    }

  type t = EGraph : _ egraph -> t

  let extract (EGraph {parentWcomp}) = parentWcomp

  let init = EGraph { count = 0;
                      available = 0;
                      parentWcomp = NodeMapCompress.empty;
                      parent = NodeMapProof.empty }

  let edges eg tv =
    let rec aux tv' intermediates =
      match NodeMapProof.find tv' eg.parent with
      | Some(tv'',e) -> aux tv'' ((tv',e)::intermediates)
      | None -> intermediates,tv'
    in
    aux tv []
                    
  module PC = struct

    type _ t = {
        pointed : Node.t; (* Node of which we have fetched the component *)
        id   : int;  (* id of component *)
        size : int;  (* size of component *)
        info : info; (* information of component *)
        root : Node.t; (* Current representative of component *)
      }

    let pp fmt pc =
      fprintf fmt "Component of %a, id=%i, size=%i, root=%a"
        Node.pp pc.pointed pc.id pc.size Node.pp pc.root
                 
    let equal : 'a t -> 'a t -> bool
      = fun t t' -> (t.id = t'.id)

    let get : 'a egraph -> Node.t -> 'a egraph * 'a t =
      fun eg pointed ->
      let rec aux tv' intermediates =
        match NodeMapCompress.find tv' eg.parentWcomp with
        | Case1 tv'' -> aux tv'' (tv'::intermediates)
        | Case2(id,size,info) ->
           let parentWcomp =
             List.fold
               (fun tv -> NodeMapCompress.add tv (Case1 tv'))
               intermediates
               eg.parentWcomp
           in
           { eg with parentWcomp = parentWcomp },
           { pointed; id; size; info; root = tv' }
           
      in aux pointed []
             
    let get_info x = x.info

  end

  let add : Node.t -> info -> _ egraph -> t
    = fun tv info eg ->
    if NodeMapCompress.mem tv eg.parentWcomp
    then EGraph eg
    else
      EGraph {
          count = eg.count+1;
          available = eg.available+1;
          parentWcomp = NodeMapCompress.add
              tv
              (Case2(eg.available,1,info))
              eg.parentWcomp;
          parent = NodeMapProof.add tv None eg.parent;
        }
             

  let update : 'a PC.t -> info -> 'a egraph -> t
    = fun pc info eg ->
    let open PC in
    EGraph { eg with parentWcomp = NodeMapCompress.add
                         pc.root
                         (Case2(pc.id,pc.size,info))
                         eg.parentWcomp }
           

  let merge : 'a PC.t -> 'a PC.t -> info -> edge -> 'a egraph -> t
    = fun pc1 pc2 info j eg ->
    Print.print ["kernel.egraph",5] (fun p ->
        p "Merging (%a) with (%a)" PC.pp pc1 PC.pp pc2);
    if PC.equal pc1 pc2
    then
      (Print.print ["kernel.egraph",5] (fun p -> p "Same component");
       update pc1 info eg)
    else
      begin
        Print.print ["kernel.egraph",5] (fun p -> p "Distinct components");
        let open PC in
        let pcsmall,pcbig =
          if pc1.size < pc2.size
          then pc1,pc2
          else pc2,pc1
        in
        let parentWcomp =
          NodeMapCompress.add
            pcbig.root (Case2(pcbig.id,pc1.size+pc2.size,info)) eg.parentWcomp
        in
        let parentWcomp =
          NodeMapCompress.add pcsmall.root (Case1 pcbig.root) parentWcomp
        in
        let rec aux tv parent = function
          | [] -> NodeMapProof.add tv (Some(pcbig.pointed,j)) parent
          | (tv',e)::l -> aux tv' (NodeMapProof.add tv (Some(tv',e)) parent) l
        in
        let intermediate,root = edges eg pcsmall.pointed in
        EGraph { eg with count = eg.count-1;
                         parentWcomp;
                         parent = aux root eg.parent intermediate }
      end
        
  module H = Hashtbl.Make(Node)

  exception DiffComp
    
  let path t pc eg =
    Print.print ["kernel.egraph",3] (fun p ->
        p "Starting path search between %a and %a" Node.pp t Node.pp PC.(pc.pointed));
    let table = H.create (3*PC.(pc.size)) in
    let table_pc = H.create (3*PC.(pc.size)) in
    H.add table t [];
    H.add table_pc PC.(pc.pointed) [];
    
    let rec aux t1 accu1 taccu b =
      Print.print ["kernel.egraph",5] (fun p ->
          p "path_aux: t1=%a accu1=%a b=%b" Node.pp t1 (List.pp pp_edge) accu1 b);
      let table1,table2 = (if b then table,table_pc else table_pc,table) in
      if H.mem table2 t1
      then
        let accu2 = H.find table2 t1 in
        if b then List.rev_append accu2 accu1
        else List.rev_append accu1 accu2
      else
        match NodeMapProof.find t1 eg.parent with
        | Some(t1',e)
          ->
           Print.print ["kernel.egraph",5] (fun p ->
               p "path_aux: found parent for %a: it's %a" Node.pp t1 Node.pp t1');
           let accu1 = e::accu1 in
             H.add table1 t1' accu1;
             begin match taccu with
             | Some(t2,accu2) -> aux t2 accu2 (Some(t1',accu1)) (not b)
             | None -> aux t1' accu1 taccu b
             end
        | None
          -> match taccu with
             | Some(t2,accu2) -> aux t2 accu2 None (not b)
             | None -> raise DiffComp
                         
    in
    let l = aux t [] (Some(PC.(pc.pointed),[])) true in
    Print.print ["kernel.egraph",5] (fun p -> p "%a" (List.pp pp_edge) l);
    l    

end

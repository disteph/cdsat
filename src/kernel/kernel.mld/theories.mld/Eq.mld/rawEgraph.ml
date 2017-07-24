open General.Sums       
open Interfaces
                                 
module Make(P : Parameters) = struct

  open P
  type node = Node.t
  module NodeMap = Map.Make(Node)

  type _ egraph = {
      (* Number of components *)
      count  : int;
      (* Parent element in e-graph, compressing paths.
         Root epresentative is mapped to the component information, to which we add:
         the id of the component and its size *)
      parentWcomp : (Node.t,int*int*info) sum NodeMap.t;
      (* Parent element, without path compression. Root is mapped to None.
         Used to produce explanations. *)
      parent : (Node.t*edge) option NodeMap.t;
    }

  type t = EGraph : _ egraph -> t

  let init = EGraph { count = 0;
                      parentWcomp = NodeMap.empty;
                      parent = NodeMap.empty }

  let edges eg tv =
    let rec aux tv' intermediates =
      match NodeMap.find tv' eg.parent with
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
                 
    let equal : 'a t -> 'a t -> bool
      = fun t t' -> (t.id = t'.id)

    let get : 'a egraph -> Node.t -> 'a egraph * 'a t =
      fun eg tv ->
      let rec aux tv' intermediates =
        match NodeMap.find tv' eg.parentWcomp with
        | Case1 tv'' -> aux tv'' (tv'::intermediates)
        | Case2(id,size,info) ->
           let parentWcomp =
             List.fold (fun tv -> NodeMap.add tv (Case1 tv')) intermediates eg.parentWcomp
           in
           { eg with parentWcomp = parentWcomp },
           { pointed = tv;
             id   = id;
             size = size;
             info = info;
             root = tv' }
           
      in aux tv []
             
    let get_info x = x.info

  end

  let add : Node.t -> info -> _ egraph -> t
    = fun tv info eg ->
    if NodeMap.mem tv eg.parentWcomp
    then EGraph eg
    else
      EGraph {
          count = eg.count+1;
          parentWcomp = NodeMap.add tv (Case2(eg.count+1,1,info)) eg.parentWcomp;
          parent = NodeMap.add tv None eg.parent;
        }
             

  let update : 'a PC.t -> info -> 'a egraph -> t
    = fun pc info eg ->
    let open PC in
    EGraph { eg with parentWcomp = NodeMap.add
                                    pc.root
                                    (Case2(pc.id,pc.size,info))
                                    eg.parentWcomp }
           

  let merge : 'a PC.t -> 'a PC.t -> info -> edge -> 'a egraph -> t
    = fun pc1 pc2 info j eg ->
    if PC.equal pc1 pc2
    then
      update pc1 info eg
    else
      let open PC in
      let pcsmall,pcbig =
        if pc1.size < pc2.size
        then pc1,pc2
        else pc2,pc1
      in
      let parentWcomp =
        NodeMap.add pcbig.root (Case2(pcbig.id,pc1.size+pc2.size,info)) eg.parentWcomp
      in
      let parentWcomp =
        NodeMap.add pcsmall.root (Case1 pcbig.root) parentWcomp
      in
      let rec aux tv parent = function
        | [] -> NodeMap.add tv (Some(pcbig.pointed,j)) parent
        | (tv',e)::l -> NodeMap.add tv (Some(tv',e))  parent
      in
      let intermediate,root = edges eg pcsmall.pointed in
      EGraph { count = eg.count-1;
               parentWcomp = parentWcomp;
               parent = aux root eg.parent intermediate }
          
  module H = Hashtbl.Make(Node)

  exception DiffComp
    
  let path t pc eg =
    let table = H.create (3*PC.(pc.size)) in
    let table_pc = H.create (3*PC.(pc.size)) in
    H.add table t [];
    H.add table_pc PC.(pc.pointed) [];
    
    let rec aux t1 accu1 taccu b =
      let table1,table2 = (if b then table,table_pc else table_pc,table) in
      if H.mem table2 t1
      then
        let accu2 = H.find table2 t1 in
        if b then List.rev_append accu1 accu2
        else List.rev_append accu2 accu1
      else
        match NodeMap.find t eg.parent with
        | Some(t1',e)
          -> let accu1 = e::accu1 in
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
    aux t [] (Some(PC.(pc.pointed),[])) true


    

end

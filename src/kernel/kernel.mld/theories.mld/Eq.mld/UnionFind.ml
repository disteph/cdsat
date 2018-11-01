open Format
open General
open Sums
open Monads
    
include UnionFind_sig

module Make(P : Parameters) = struct

  open P

  module PC = struct

    type t = {
      pointed : Node.t; (* Node of which we have fetched the component *)
      id   : int;  (* id of component *)
      size : int;  (* size of component *)
      info : info; (* information of component *)
      root : Node.t; (* Current representative of component *)
    }

    let pp fmt pc =
      fprintf fmt "Component of %a, id=%i, size=%i, root=%a"
        Node.pp pc.pointed pc.id pc.size Node.pp pc.root

    let equal : t -> t -> bool
      = fun t t' -> (t.id = t'.id)

  end

  module Cache = Map.Make(Node)
  
  type state = {
      (* Number of components *)
      count : int;
      (* First available component id *)
      available : int;
      (* Parent element in e-graph, compressing paths.
         Root representative is mapped to the component information, to which we add:
         the id of the component and its size *)
      parentWcomp : NodeMapCompress.t;
      (* Whenever a find has been done, not only is a compression performed
         but also an immediate cache of the last finds are recorded;
         cache is flushed every time the e-graph is modified by a merge *)
      cache : PC.t Cache.t;
      (* Parent element, without path compression. Root is mapped to None.
         Used to produce explanations. *)
      parent : NodeMapProof.t;
    }

  module EMonad = StateMonad(struct type t = state end)
  include Make_Let(EMonad)

  type t = state

  let extract eg = eg.parentWcomp, eg

  let init = { count = 0;
               available = 0;
               parentWcomp = NodeMapCompress.empty;
               cache  = Cache.empty;
               parent = NodeMapProof.empty }

  let force a = a

  let elazy a eg = lazy (a eg |> fst),eg
  
  let edges tv eg =
    let rec aux tv' intermediates =
      match NodeMapProof.find tv' eg.parent with
      | Some(tv'',e) -> aux tv'' ((tv',e)::intermediates)
      | None -> intermediates,tv'
    in
    aux tv []
                    
  let get : Node.t -> PC.t EMonad.t =
    fun pointed eg ->
      (* We first look in the cache if we have already computed it *)
      if Cache.mem pointed eg.cache then Cache.find pointed eg.cache, eg
      else (* Cache miss *)
        let rec aux tv' intermediates =
          match NodeMapCompress.find tv' eg.parentWcomp with
          | Case1 tv'' -> aux tv'' (tv'::intermediates)
          | Case2(id,size,info) ->
            let pc = PC.{ pointed; id; size; info; root = tv' } in
            let parentWcomp =
              List.fold
                (fun tv -> NodeMapCompress.add tv (Case1 tv'))
                intermediates
                eg.parentWcomp
            in
            let cache = Cache.add pointed pc eg.cache in
            pc, { eg with parentWcomp; cache }

        in aux pointed []

  let are_connected node1 node2 =
    let%bind pc1 = get node1 in
    let%map  pc2 = get node2 in
    PC.equal pc1 pc2
      
  let get_info tv = let%map pc = get tv in pc.info

  let add : Node.t -> info -> unit EMonad.t = fun tv info eg ->
    if NodeMapCompress.mem tv eg.parentWcomp
    then (),eg (* Warning: new info is lost. We keep the old. *)
    else (),{
        count = eg.count+1;
        available = eg.available+1;
        parentWcomp = NodeMapCompress.add
            tv
            (Case2(eg.available,1,info))
            eg.parentWcomp;
        cache  = eg.cache; (* The cache is not invalidated by new singleton component *)
        parent = NodeMapProof.add tv None eg.parent;
      }
             

  let update node info =
    let%bind pc = get node in
    let open PC in
    fun eg ->
      (),
      { eg with parentWcomp = NodeMapCompress.add
                    pc.root
                    (Case2(pc.id,pc.size,info))
                    eg.parentWcomp;
                cache = Cache.empty (* Cache is invalidated *)
      }


  let merge node1 node2 info j
    =
    let%bind pc1 = get node1 in
    let%bind pc2 = get node2 in
    Print.print ["kernel.egraph",5] (fun p ->
        p "Merging (%a) with (%a)" PC.pp pc1 PC.pp pc2);
    if PC.equal pc1 pc2
    then
      (Print.print ["kernel.egraph",5] (fun p -> p "Same component");
       update node1 info)
    else
      begin
        Print.print ["kernel.egraph",5] (fun p -> p "Distinct components");
        let open PC in
        let pcsmall,pcbig =
          if pc1.size < pc2.size
          then pc1,pc2
          else pc2,pc1
        in
        fun eg ->
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
          let intermediate,root = edges pcsmall.pointed eg in
          (),
          { eg with count = eg.count-1;
                    parentWcomp;
                    parent = aux root eg.parent intermediate;
                    cache = Cache.empty (* Cache is invalidated *) }
      end
        
  module H = Hashtbl.Make(Node)

  type path = { first : Node.t;
                tail  : (edge*Node.t) list } [@@deriving show]

  let path_tail {tail} =
    match tail with
    | [] -> failwith "tail of empty path!"
    | (e,first)::tail -> {first;tail}

  let path_cons (e,node1,node2) {first; tail} =
    assert(Node.equal node2 first);
    { first = node1; tail = (e,node2)::tail }

  let rec path_rev_append p1 p2 =
    assert(Node.equal p1.first p2.first);
    match p1.tail with
    | [] -> p2
    | (e,first)::l1 -> path_rev_append { first; tail = l1 } { first; tail = (e,p2.first)::p2.tail }

  exception DiffComp
  
  let get_path node1 node2 = (* we assume node1 and node2 are connected *)
    let%bind pc = get node1 in
    fun eg ->
      Print.print ["kernel.egraph",3] (fun p ->
          p "Starting path search between %a and %a" Node.pp node1 Node.pp node2);
      let table1 = H.create (3*PC.(pc.size)) in
      let table2 = H.create (3*PC.(pc.size)) in
      let init1 = { first = node1; tail = [] } in
      let init2 = { first = node2; tail = [] } in
      H.add table1 node1 init1;
      H.add table2 node2 init2;
      (* tablei is a map that maps a node to a path from that node to nodei *)

      (* Pre-condition: accu1 is path to node_i,
         symmetric, if present, is a path accu2 to node_j
         (i,j)=(1,2) if b, (i,j)=(2,1) if not *)
      let rec aux accu1 ?symmetric b =
        Print.print ["kernel.egraph",5] (fun p ->
            p "get_path_aux: accu1=%a b=%b" pp_path accu1 b);
        let table1,table2 = (if b then table1,table2 else table2,table1) in
        let t1 = accu1.first in
        if H.mem table2 t1
        then
          let accu2 = H.find table2 t1 in
          if b then path_rev_append accu1 accu2
          else path_rev_append accu2 accu1
        else
          match NodeMapProof.find t1 eg.parent with
          | Some(t1',e) ->
            Print.print ["kernel.egraph",5] (fun p ->
                p "path_aux: found parent for %a: inode1s %a" Node.pp t1 Node.pp t1');
            let accu1 = path_cons (e,t1',t1) accu1 in
            H.add table1 t1' accu1;
            begin match symmetric with
              | Some accu2 -> aux accu2 ~symmetric:accu1 (not b) (* We swap *)
              | None       -> aux accu1 b (* We don't swap *)
            end
          | None ->
            match symmetric with
            | Some accu2 -> aux accu2 (not b)
            | None -> raise DiffComp

      in
      let l = aux init1 ~symmetric:init2 true in
      Print.print ["kernel.egraph",5] (fun p -> p "%a" pp_path l);
      l,eg

end

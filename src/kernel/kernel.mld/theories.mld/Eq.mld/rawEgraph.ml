open General.Sums       
open Interfaces
                                 
module Make(P : Parameters) = struct

  open P
  type node = Node.t
  module NodeMap = Map.Make(Node)
                           
  type _ egraph = {
      (* Number of components *)
      count  : int;
      (* Next element of component.
           Representative is mapped to the component information, to which we add:
           the id of the component and its size *)
      next   : (Node.t,int*int*info) sum NodeMap.t;
      (* All E-graph edges, bidirectional. Used to compute explanations. *)
      others : edge NodeMap.t NodeMap.t;
    }

  type t = EGraph : _ egraph -> t (* [@@unboxed] *)

  let init = EGraph { count = 0; next = NodeMap.empty; others = NodeMap.empty }

  module PC = struct

    type _ t = {
        pointed : Node.t; (* Node of which we have fetched the component *)
        id   : int;  (* id of component *)
        size : int;  (* size of component *)
        info : info; (* information of component *)
        representative : Node.t; (* Current representative of component *)
        intermediates  : Node.t list; (* Intermediate nodes seen from pointed to representative *)
      }

                 
    let equal : 'a t -> 'a t -> bool
      = fun t t' -> (t.id = t'.id)

    let get : 'a egraph -> Node.t -> 'a t =
      fun eg tv ->
      let rec aux tv' intermediates =
        match NodeMap.find tv' eg.next with
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

    let get_info x = x.info

  end

  let add : Node.t -> info -> _ egraph -> t
    = fun tv info eg ->
    if NodeMap.mem tv eg.next
    then EGraph eg
    else
      EGraph
        { eg with
          count = eg.count+1;
          next  = NodeMap.add
                    tv
                    (Case2(eg.count+1,1,info))
                    eg.next }
        

  let update : 'a PC.t -> info -> 'a egraph -> t
    = fun pc info eg ->
    EGraph
      { eg with next = NodeMap.add
                         pc.PC.representative
                         (Case2(pc.PC.id,pc.PC.size,info))
                         eg.next }
      
  let tvmap_add t1 t2 j map =
    let prev =
      if NodeMap.mem t1 map
      then NodeMap.find t1 map
      else NodeMap.empty
    in
    NodeMap.add t1 (NodeMap.add t2 j prev) map

  let merge : 'a PC.t -> 'a PC.t -> info -> edge -> 'a egraph -> t
    = fun pc1 pc2 info j eg ->
    if PC.equal pc1 pc2
    then
      update pc1 info eg
    else
      let open PC in
      let eg =
        if Node.equal pc1.pointed pc2.pointed
        then eg
        else { eg with
               others = tvmap_add pc1.pointed pc2.pointed j
                          (tvmap_add pc2.pointed pc1.pointed j eg.others)
             }
      in
      let rec aux next pckeep tv = function
        | [] ->
           let newsize = pc1.size + pc2.size in
           let next = NodeMap.add pckeep.representative (Case2(pckeep.id,newsize,info)) next in
           let next = NodeMap.add tv (Case1 pckeep.pointed) next in
           EGraph { eg with count = eg.count-1; next = next }
        | tv'::intermediates ->
           let next = NodeMap.add tv (Case1 tv') next in
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

(* An implementation of Persistent Union Find *)

module Make(Ord: Map.OrderedType): (Interfaces.PersistentUnionFind with type e = Ord.t) = struct

  module FCA = MyFCA.Make(struct
      type t = Ord.t
      let equal a b = (Ord.compare a b =0)
  end)

  module M = Map.Make(Ord)

  type e = Ord.t

  type 'a t = {father : (((e*'a) option)*int) M.t;
               sons   : e list M.t;
               memo   : FCA.t M.t ref}

  let create = {father = M.empty;
                sons = M.empty;
                memo = ref M.empty }

  let add m i = 
    try 
      let _ = M.find i m.father in
      m
    with Not_found ->
      {father = M.add i (None,0) m.father; 
       sons = M.add i [] m.sons;
       memo = ref M.empty}

  let rec find_aux m i =
  (* print_string "find_aux\n";*)
    match M.find i m.father with
    | None,height   -> i,height
    | Some(k,_),_ -> find_aux m k

  let find m i = fst (find_aux m i)

  (* remove an element from a list
     suppose that it appears only once *)
  let rec remove i = function
    | [] -> []
    | a::ta when Ord.compare a i =0 -> ta
    | a::ta -> a::(remove i ta)

  (* In m, revert an arc i->j with label d
     Pre-condition: j is a root in m
     k is an estimation of the height of the subtree of i *)
  let revert i j d k m =
  (* print_string ("revert "^(string_of_int i)^"->"^(string_of_int j)^"\n");*)
    match M.find j m.father with
    | None,k' ->
       {father = M.add i (None,k+k') (M.add j (Some(i,d),k') m.father);
        sons = M.add i (j::(M.find i m.sons)) (M.add j (remove i (M.find j m.sons)) m.sons);
        memo = ref M.empty}
    | Some _,_ -> failwith "j should be the root"

  (* make i the root of its class by reverting the arcs on the path 
     from i to its representative *)
  let reroot i m =
    (* print_string "reroot\n";*)
    (* Invariant: in a call to (aux x cont), cont is only called with
       an argument where x is a root *)
    let rec aux x cont =
      match M.find x m.father with
      | None,  _    -> cont m
      | Some(j,d),k -> aux j (fun m -> cont (revert x j d k m))
    in
    aux i (fun m -> m)

  let addLink m i j d =
    (* print_string ("addLink "^(string_of_int i)^"->"^(string_of_int j)^"\n");*)
    if Ord.compare i j =0 then m else
      let m = add m j in
      if Ord.compare (find m i) (find m j) =0 then m 
      else
        let m'  = reroot i m in
	let m'' = reroot j m' in
	let _,k = M.find i m''.father in
	let _,k' = M.find j m''.father in
	{father = M.add i (Some(j,d), max k k') m''.father;
	 sons = M.add j (i::(M.find j m''.sons)) m''.sons;
         memo = ref M.empty}

  (* union pas utilis'e *)
  (* let union m i j = *)
  (* (\* print_string "union\n";*\) *)
  (*   let ci, ri = find_aux m i in *)
  (*   let cj, rj = find_aux m j in *)
  (*   if Ord.compare ci cj =0 then m *)
  (*   else if ri < rj then  *)
  (*     {father = M.add ci (Some cj,rj,None) m.father; *)
  (*      sons = M.add cj (ci::(M.find cj m.sons)) m.sons; *)
  (*      memo = ref M.empty} *)
  (*   else if rj < ri then  *)
  (*     {father = M.add cj (Some ci,ri,None) m.father; *)
  (*      sons = M.add ci (cj::(M.find ci m.sons)) m.sons; *)
  (*      memo = ref M.empty} *)
  (*   else  *)
  (*     {father = M.add ci (Some cj,rj+1,None) m.father; *)
  (*      sons = M.add cj (ci::(M.find cj m.sons)) m.sons; *)
  (*      memo = ref M.empty} *)

  let rec pathTo m i j = 
   (* print_string "pathTo\n"; *)
    match j, M.find i m.father with
    | Some j',_ when Ord.compare i j' =0 -> []
    | None, (None,  _) -> []
    | _, (Some(k,d),_) -> d::(pathTo m k j)
    | Some _, (None,_) -> failwith ("the third argument isn't an ancestor of the second one ")

  let sonsList i m map = 
   (* print_string "sonsList\n"; *)
    List.map
      (fun e -> M.find e map)
      (M.find i m.sons)

  (* build the tree for the class of the representative r *)
  let build r m =
    (* print_string "build\n";*)
    (* get all the nodes of the tree *)
    let rec aux n l =
      let rec aux2 accu = function
        | [] -> accu
        | hd::tl -> aux2 (aux hd accu) tl
      in aux2 (n::l) (M.find n (m.sons))
    in
    let list = aux r [] in
    (* Create map from nodes to integer indices *)
    let _,idmap = 
      List.fold (fun n (i,idmap) -> (i+1, M.add n i idmap)) list (0,M.empty)
    in
    (* create the tree and add the arcs *)
    let _,t =
      List.fold
        (fun n (i,t) ->
          let x,_ = M.find n m.father in
          match x with
          | None -> i+1, FCA.add i i (sonsList n m idmap) t
          | Some(j,_) -> i+1, FCA.add i (M.find j idmap) (sonsList n m idmap) t
        )
        list
        (0,FCA.create list)
    in
    (* Memoising in m.memo the construction, for future re-use *)
    m.memo := List.fold (fun n tmap -> M.add n t tmap) list !(m.memo);
    t

  let fca m i j = 
    (* print_string "fca\n";*)
    let t =
      try M.find i !(m.memo)
      with Not_found -> build (find m i) m 
    in
    let i',j' = FCA.ind i j t in
    (* print_int i; print_int j; print_int (FCA.fca t i' j'); print_newline();*)
    FCA.fca t i' j'
    
  let rec explain u a b =
    (* we compute the first common ancestor *)
    let r = fca u a b in 
    (* and the explaination on the paths to it *)
    List.append (pathTo u a (Some r)) (pathTo u b (Some r))

end

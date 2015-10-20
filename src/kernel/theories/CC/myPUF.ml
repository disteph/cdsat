(* An implementation of Persistent Union Find *)

module Make(Ord: Map.OrderedType) = struct

  module M = Map.Make(Ord)

  type e = Ord.t

  type 'a t = {father : ((e option)*int*('a option)) M.t;
               sons   : (e list) M.t}

  let create = {father = M.empty;
                sons = M.empty}

  let add m i = 
    try 
      let _ = M.find i m.father in
      m
    with Not_found ->
      {father = M.add i (None,0,None) m.father; 
       sons = M.add i [] m.sons}

  let rec print_list = function
    | []    -> print_newline();
    | a::ta -> print_string ((string_of_int a)^" "); print_list ta

  let rec find_aux m i =
  (* print_string "find_aux\n";*)
    match M.find i m.father with
    | None,j,_   -> i,j
    | Some k,j,_ -> find_aux m k

  let find m i = fst (find_aux m i)

(* remove an element from a list
   suppose that it appears only once *)
  let rec remove i = function
    | [] -> []
    | a::ta when a = i -> ta
    | a::ta -> a::(remove i ta)

(* revert an arc i->j with the label d 
   k is an estimation of the height of the subtree of i *)
  let revert i j d k m =
  (* print_string ("revert "^(string_of_int i)^"->"^(string_of_int j)^"\n");*)
    let _,k',_ = M.find j m.father in
    {father = M.add i (None,k+k',None) (M.add j (Some i,k',d) m.father);
     sons = M.add i (j::(M.find i m.sons)) 
        (M.add j (remove i (M.find j m.sons)) m.sons)}

(* make i the root of its class by reverting the arcs on the path 
   from i to its representative *)
  let reroot i m =
    (*    print_string "reroot\n";*)
    let rec aux x cont =
      match M.find x m.father with
      | None,  _,_ -> cont m
      | Some j,k,d -> aux j (fun m -> cont (revert x j d k m))
    in
    aux i (fun m -> m)

  let addLink_aux m i j d =
  (* print_string ("addLink "^(string_of_int i)^"->"^(string_of_int j)^"\n");*)
    if i = j then m else
      try 
        begin match M.find j m.father with
        | Some k,_,_ when k = i -> m
        | _ -> if find m i = find m j then m else
	    begin
	      match M.find i m.father with
	      | Some k,_,_ when k = j -> m
	      | _ -> let m' = reroot i m in
		     let m'' = reroot j m' in
		     let _,k,_ = M.find i m''.father in
		     let _,k',_ = M.find j m''.father in
		     {father = M.add i (Some j,(max k k'),d) m''.father;
		      sons = M.add j (i::(M.find j m''.sons)) m''.sons}
	    end
        end 
      with Not_found -> let m' = reroot i m in
		        let m'' = add m' j in
		        let _,k,_ = M.find i m''.father in
		        let _,k',_ = M.find j m''.father in
		        {father = M.add i (Some j,(max k k'),d) m''.father;
		         sons = M.add j (i::(M.find j m''.sons)) m''.sons}

  let addLink m i j d = addLink_aux m i j (Some d)

  let union m i j =
  (* print_string "union\n";*)
    let ci, ri = find_aux m i in
    let cj, rj = find_aux m j in
    if ci = cj then m
    else if ri < rj then 
      {father = M.add ci (Some cj,rj,None) m.father;
       sons = M.add cj (ci::(M.find cj m.sons)) m.sons}
    else if rj < ri then 
      {father = M.add cj (Some ci,ri,None) m.father;
       sons = M.add ci (cj::(M.find ci m.sons)) m.sons}
    else 
      {father = M.add ci (Some cj,rj+1,None) m.father;
       sons = M.add cj (ci::(M.find cj m.sons)) m.sons}

  let rec path m i =
  (* print_string "path\n";*)
    match M.find i m.father with
    | None,_,_ -> []
    | Some j,_,None -> path m j
    | Some j,_,Some d -> d::(path m j)

  let rec pathTo m i j =
  (*    print_string "pathTo\n";*)
  (*    print_int i; print_int j;
        M.iter (fun k (x,y,z) -> match x with Some p -> print_string ("("^(string_of_int k)^","^(string_of_int p)^") ")
	| None -> print_string ("("^(string_of_int k)^","^(string_of_int k)^") ")) m.father ;
        print_newline();*)
    if i = j then [] else
      match M.find i m.father with
      | None,_,_ -> failwith ("the third argument isn't an ancestor of the second one ")
      | Some k,_,None when k = j -> []
      | Some k,_,Some d when k = j -> [d]
      | Some k,_,None -> pathTo m k j
      | Some k,_,Some d -> d::(pathTo m k j)

  let sonsList i m map = 
   (* print_string "sonsList\n"; *)
    List.map
      (fun e -> M.find e map)
      (M.find i m.sons)

  let tmap = ref M.empty

(* build the tree for the class of the representative r *)
  let build r m =
  (* print_string "build\n";*)

  (* get all the nodes of the tree *)
    let rec aux n l =
    (* print_int n; print_string " ";*)
      let rec aux2 accu = function
        | [] -> accu
        | hd::tl -> aux2 (aux hd accu) tl
      in aux2 (n::l) (M.find n (m.sons))

    in
  (*    M.iter (fun k (x,y,z) -> match x with Some p -> print_string ("("^(string_of_int k)^","^(string_of_int p)^") ")
	| None -> print_string ("("^(string_of_int k)^","^(string_of_int k)^") ")) m.father ;
        print_newline();*)
  (* print_newline();*)
    let list = aux r [] in
  (* Create map from nodes to integer indices *)
    let _,idmap = 
      List.fold_left (fun (i,idmap) n -> (i+1, M.add n i idmap)) (0,M.empty) list
    in
  (* create the tree and add the arcs *)
    let _,t =
      List.fold_left 
        (fun (i,t) n ->
          let x,y,z = M.find n m.father in
          match x with
          | None -> i+1, MyFCA.add i i (sonsList n m idmap) t
          | Some j -> i+1, MyFCA.add i (M.find j idmap) (sonsList n m idmap) t
        )
        (0,MyFCA.create list)
        list
    in
    (* Memoising in tmap the construction, for future re-use *)
    List.iter (fun n -> tmap := M.add n t !tmap) list;
    t

  let fca m i j = 
  (* print_string "fca\n";*)
    try
      let t = M.find i !tmap in
      let i',j' = MyFCA.ind i j t in
    (* print_int i; print_int j; print_int (MyFCA.fca t i' j'); print_newline();*)
      MyFCA.fca t i' j'
    with Not_found ->
      let r = find m i in
      let t = build r m in
      let i',j' = MyFCA.ind i j t in
    (* print_int i; print_int j; print_int (MyFCA.fca t i' j'); print_newline();*)
      MyFCA.fca t i' j'

  let clear () = tmap := M.empty; MyFCA.clear ()

end

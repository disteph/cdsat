open Interfaces

module Ord = struct
    
  type t = int

  let compare = Pervasives.compare

end

module M = Map.Make (Ord)

module PUnionFind = struct

  type e = int

  type 'a t = {father : ((e option)*int*('a option)) M.t; sons : (e list) M.t}

  let create = {father = M.empty; sons = M.empty}

  let add m i = 
    try 
      let _ = M.find i m.father in
      m
    with Not_found ->
      {father = M.add i (None,0,None) m.father; 
       sons = M.add i [] m.sons}

  let rec print_list = function
    | [] -> print_newline();
    | (a::ta) -> print_string ((string_of_int a)^" "); print_list ta

  let rec find_aux m i =
(*    print_string "find_aux\n";*)
    match (M.find i m.father) with
    | (None,j,_) -> (i,j)
    | (Some k,j,_) -> find_aux m k

  let find m i = (fst (find_aux m i))

    (* remove an element from a list
       suppose that it appears only once *)
  let rec remove i = function
    | [] -> []
    | (a::ta) when a = i -> ta
    | (a::ta) -> a::(remove i ta)

  (* revert an arc i->j with the label d 
     k is an estimation of the height of the subtree of i *)
  let revert i j d k m =
(*    print_string ("revert "^(string_of_int i)^"->"^(string_of_int j)^"\n");*)
    let (_,k',_) = (M.find j m.father) in
    {father = M.add i (None,k+k',None) (M.add j (Some i,k',d) m.father);
     sons = M.add i (j::(M.find i m.sons)) 
	(M.add j (remove i (M.find j m.sons)) m.sons)}

    (* make i the root of its class by reverting the arcs on the path 
       from i to its representative *)
  let reroot i m =
(*    print_string "reroot\n";*)
    let rec aux m = function
      | [] -> m
      | ((i,j,d,k)::t) -> aux (revert i j d k m) t
    in
    let l = ref [] and x = ref i and b = ref true in
    while !b do
      match (M.find !x m.father) with
      | (None,_,_) -> b := false
      | (Some j,k,d) -> l := (!x,j,d,k)::(!l); x := j
    done;
    aux m !l

  let addLink m i j d =
(*    print_string ("addLink "^(string_of_int i)^"->"^(string_of_int j)^"\n");*)
    if i = j then m else
      try 
	begin match (M.find j m.father) with
	| (Some k,_,_) when k = i -> m
	| _ -> if (find m i) = (find m j) then m else
	    begin
	      match (M.find i m.father) with
	      | (Some k,_,_) when k = j -> m
	      | _ -> let m' = reroot i m in
		     let m'' = reroot j m' in
		     let (_,k,_) = M.find i m''.father in
		     let (_,k',_) = M.find j m''.father in
		     {father = M.add i (Some j,(max k k'),d) m''.father;
		      sons = M.add j (i::(M.find j m''.sons)) m''.sons}
	    end
	end 
      with Not_found -> let m' = reroot i m in
			let m'' = add m' j in
			let (_,k,_) = M.find i m''.father in
			let (_,k',_) = M.find j m''.father in
			{father = M.add i (Some j,(max k k'),d) m''.father;
			 sons = M.add j (i::(M.find j m''.sons)) m''.sons}

  let union m i j =
(*    print_string "union\n";*)
    let (ci, ri) = find_aux m i in
    let (cj, rj) = find_aux m j in
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
(*    print_string "path\n";*)
    match (M.find i m.father) with
    | (None,_,_) -> []
    | (Some j,_,None) -> path m j
    | (Some j,_,Some d) -> d::(path m j)

  let rec pathTo m i j =
(*    print_string "pathTo\n";*)
(*    print_int i; print_int j;
M.iter (fun k (x,y,z) -> match x with Some p -> print_string ("("^(string_of_int k)^","^(string_of_int p)^") ")
	| None -> print_string ("("^(string_of_int k)^","^(string_of_int k)^") ")) m.father ;
print_newline();*)
    if i = j then [] else
      match (M.find i m.father) with
      | (None,_,_) -> failwith ("the third argument "^(string_of_int j)^" isn't an ancestor of the second one "^(string_of_int i))
      | (Some k,_,None) when k = j -> []
      | (Some k,_,Some d) when k = j -> [d]
      | (Some k,_,None) -> pathTo m k j
      | (Some k,_,Some d) -> d::(pathTo m k j)

  let sonsList i m map = 
(*    print_string "sonsList\n";*)
    let l = ref (M.find i m.sons) and r = ref [] in
    while !l<>[] do
      r := (M.find (List.hd !l) map)::(!r);
      l := (List.tl !l)
    done;
    !r

  let tmap = ref M.empty

(* build the tree for the class of the representative r *)
  let build r m =
(*    print_string "build\n";*)
    let l = ref [] in
(* get all the nodes of the tree *)
    let rec aux n =
(*      print_int n; print_string " ";*)
      l := n::(!l);
      let sn = ref (M.find n (m.sons)) in
(*      print_list !sn;*)
      while !sn<>[] do
	aux (List.hd !sn);
	sn := (List.tl !sn)
      done
    in
(*    M.iter (fun k (x,y,z) -> match x with Some p -> print_string ("("^(string_of_int k)^","^(string_of_int p)^") ")
	| None -> print_string ("("^(string_of_int k)^","^(string_of_int k)^") ")) m.father ;
print_newline();*)
    aux r;
(*    print_newline();*)
    let a = Array.of_list !l in
    let l = Array.length a in
    let idmap = ref M.empty in
(* create the tree *)
    let t = ref (Fca.create a) in
    for i = 0 to l-1 do
      idmap := M.add a.(i) i !idmap
    done;
(* add the arcs *)
    for i = 0 to l-1 do
      let (x,y,z) = (M.find a.(i) m.father) in
      match x with
      | None -> t := Fca.add i i (sonsList a.(i) m !idmap) !t
      | (Some j) -> t := Fca.add i (M.find j !idmap) (sonsList a.(i) m !idmap) !t
    done;
    for i = 0 to l-1 do
      tmap := M.add a.(i) !t !tmap
    done;
    !t

  let fca m i j = 
(*    print_string "fca\n";*)
    try
      let t = M.find i !tmap in
      let (i',j') = Fca.ind i j t in
(*      print_int i; print_int j; print_int (Fca.fca t i' j'); print_newline();*)
      Fca.fca t i' j'
    with Not_found ->
      let r = find m i in
      let t = build r m in
      let (i',j') = Fca.ind i j t in
(*      print_int i; print_int j; print_int (Fca.fca t i' j'); print_newline();*)
      Fca.fca t i' j'

  let clear () = tmap := M.empty; Fca.clear ()

end

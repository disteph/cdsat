module Make(M: sig
  type t
  val equal: t -> t -> bool 
end) =
struct

  type t = { nodes : M.t array;
             father : int array;
             sons : (int list) array; 
	     id : int }

  let idr = ref 0

  let create list =
    let a = Array.of_list list in
    let l = Array.length a in
    incr idr;
    {nodes = a; father = Array.make l 0; sons = Array.make l []; id = !idr}

  let add i f s t =
    let fa = t.father in
    let sa = t.sons in
    fa.(i) <- f;
    sa.(i) <- s;
    {nodes = t.nodes; father = fa; sons = sa; id = t.id}

  let numNodes t = Array.length t.nodes

  let ind i j t = 
    let i' = ref 0 and j' = ref 0 and bi = ref true and bj = ref true in
    for k = 0 to (numNodes t)-1 do
      if M.equal t.nodes.(k) i then (i' := k; bi := false);
      if M.equal t.nodes.(k) j then (j' := k; bj := false);
    done;
    if !bi then
      failwith ("the first argument isn't in the tree")
    else if !bj then
      failwith ("the second argument isn't in the tree")
    else 
      !i',!j'

  let root t = 
    let x = ref t.father.(0) in
    while t.father.(!x) <> !x do
      x := t.father.(!x)
    done;
    !x
      
  let father t x = t.father.(x)
    
  let sons t x = t.sons.(x)

  let print_tree t = 
    for i = 0 to (numNodes t)-1 do
      print_string ((string_of_int (father t i))^" ")
    done;
    print_newline()

  let eulerianTour t =
    let r = root t in
    let e = Array.make (2*(numNodes t)-1) (r,0) in
    let pos = ref 0 in
    let rec aux n d =
      e.(!pos) <- (n,d);
      incr pos;
      let l = ref (sons t n) in
      while !l<>[] do
        aux (List.hd !l) (d+1);
        e.(!pos) <- (n,d);
        incr pos;
        l := List.tl !l
      done
    in aux r 0;
    e

  let fstOccur t e =
    let n = (numNodes t) in
    let r = Array.make n 0 in
    for i = 0 to (2*n-2) do
      if r.(fst e.(i)) = 0 && (fst e.(i)) <> (fst e.(0)) then r.(fst e.(i)) <- i
    done;
    r

  let preIminForFCA e = 
    let n = (Array.length e) in
    let k = (int_of_float ((log (float_of_int n))/.(log 2.))) in
    let m = (Array.make_matrix n (k+1) 0) in
    for i = 0 to n-1 do
      m.(i).(0) <- i
    done;
    let x = ref 1 in
    for j = 1 to k do
      for i = 0 to n-1 do
        if i + !x < n then
	  m.(i).(j) <- if (snd e.(m.(i).(j-1)))<(snd e.(m.(i + !x).(j-1)))
	    then m.(i).(j-1) 
	    else m.(i + !x).(j-1)
        else 
	  m.(i).(j) <- m.(i).(j-1)
      done;
      x:=2*(!x)
    done;
    m

  let rec power x = function
    | 0 -> 1
    | n when (n mod 2) = 0 -> power (x*x) (n/2)
    | n -> x*(power (x*x) (n/2))

  let imin i j m e =
    let p = (int_of_float ((log (float_of_int ((max i j)-(min i j)+1)))/.(log 2.))) in
    let r = power 2 p in
    let m1 = m.(min i j).(p) in
    let m2 = m.((max i j)+1-r).(p) in
    if (snd e.(m1)) < (snd e.(m2)) then m1 else m2

  module Ord = struct
      
    type t = int

    let compare = Pervasives.compare

  end

  module M = Map.Make (Ord)

  let map = ref M.empty

  let print_array t = 
    let n = (Array.length t) in
    for i = 0 to n-1 do
      print_int t.(i);
      print_string " "
    done;
    print_newline()

  let print_matrix m =
    let n = Array.length m in
    for i = 0 to n-1 do
      print_array m.(i)
    done;
    print_newline()

  let rec fca t i j = 
    try 
      let (e,r,m) = M.find t.id !map in
    (*    print_int i; print_int j; print_newline();
          print_array (Array.map (fun x -> fst x) e);
          print_array r;
          print_array (Array.map (fun x -> snd x) e);
          print_int r.(i); print_int r.(j);
          print_int (imin r.(i) r.(j) m e); print_newline();
          print_matrix m;*)
      t.nodes.(fst e.(imin r.(i) r.(j) m e))
    with Not_found ->
      let e = eulerianTour t in
      let r = fstOccur t e in
      let m = preIminForFCA e in
  (*  print_int i; print_int j; print_newline();
      print_array (Array.map (fun x -> fst x) e);
      print_array r;
      print_array (Array.map (fun x -> snd x) e);
      print_int r.(i); print_int r.(j);
      print_int (imin r.(i) r.(j) m e); print_newline();
      print_matrix m;*)
      map := M.add t.id (e,r,m) !map;
      t.nodes.(fst e.(imin r.(i) r.(j) m e))
(*    fca t i j*)

  let clear () = map := M.empty; idr := 0

end

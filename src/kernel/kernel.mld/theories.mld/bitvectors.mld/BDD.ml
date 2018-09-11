open Format

open Top.Basic

include MLBDD

let getM a t = if IntMap.mem a t then IntMap.find a t else []

let compile x = 
  let ident = ref 0 in
  let aux y =
    match y with
    | BFalse -> IntMap.empty, false, -2
    | BTrue  -> IntMap.empty, true, -1
    | BIf((fmap,_,fid),var,(tmap,_,tid)) ->
       let map = IntMap.union (fun _ s1 s2 -> Some(List.append s1 s2)) fmap tmap in
       incr ident;
       IntMap.add var ((!ident,fid,tid)::(getM var map)) map,true,!ident
  in
  foldb aux x

let ratio a b = (float_of_int a) /. (float_of_int b)
                                      
let pp pp_node fmt t =
  let map,b,_ = compile t in
  let height = IntMap.cardinal map in
  fprintf fmt "\\begin{tikzpicture}[xscale=4,yscale=4.5,auto,swap]\n";
  if height>0 || not b then 
    fprintf fmt "\\node[minimum size=20pt,inner sep=0pt] (-2) at (0,-1) {F};\n";
  if height>0 || b then 
    fprintf fmt "\\node[minimum size=20pt,inner sep=0pt] (-1) at (1,-1) {T};\n";
  (* print_endline(string_of_int vars); *)
  let aux var l m =
    let nodes = List.length l + 1 in
    (* print_endline(string_of_int var ^": "^ string_of_int nodes); *)
    let aux_level n (i,_,_) =
      fprintf fmt
        "\\node[minimum size=20pt,inner sep=0pt] (%i) at (%f,%f) {%a};\n"
        i
        (ratio (n+1) nodes)
        (ratio (-m) height)
        pp_node var
    in
    List.iteri aux_level l;
    m+1
  in
  let _ = IntMap.fold aux map 0 in
  let aux var l =
    let aux_level (i,j,k) =
      fprintf fmt "\\path[draw,dashed,->] (%i) -- node {} (%i);\n" i j;
      fprintf fmt "\\path[draw,->] (%i) -- node {} (%i);\n" i k
    in
    List.iter aux_level l
  in
  IntMap.iter aux map;
  fprintf fmt "\\end{tikzpicture}"
          
let to_string_aux a =
  let buf = Buffer.create 255 in
  let fmt = Format.formatter_of_buffer buf in
  a (Format.fprintf fmt);
  Format.fprintf fmt "%!";
  Buffer.contents buf
                  
let to_string pp_node a = to_string_aux (fun p->p "%a" (pp pp_node) a)
                                        
let man = MLBDD.init()
let dtrue = dtrue man
let dfalse = dfalse man
let ithvar = ithvar man
                 
(* let f = MLBDD.ithvar man 79 *)
(* (\* let g = BDD.reveal f *\) *)
(* let () = print_endline(MLBDD.to_string f) *)
(* let _ = MLBDD.id f *)
(* let _ = match MLBDD.inspectb(f) with *)
(*   | MLBDD.BIf(g,i,d) -> MLBDD.inspectb g, MLBDD.inspectb d *)
(*   | _ -> failwith "" *)

(* let fmap,_,_ = BDD.compile f *)
(* let _ = BDD.IntMap.cardinal fmap *)

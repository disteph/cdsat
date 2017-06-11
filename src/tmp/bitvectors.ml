open HardCaml.Bits.Comb.IntbitsList
let a = width (const "12'd0")


module BDD = struct

  include MLBDD

  module M = Map.Make(struct type t = int let compare = compare end)
      
  let getM a t = if M.mem a t then M.find a t else []

  (* let compile_old x =  *)
  (*   let visited = ref M.empty in *)
  (*   let rec aux y = *)
  (*     if M.mem (id y) !visited then (M.empty,true) *)
  (*     else *)
  (*       begin *)
  (*         (\* print_endline("Visiting "^string_of_int (id y)); *\) *)
  (*         visited := M.add (id y) () !visited; *)
  (*         match inspectb y with *)
  (*         | BFalse -> M.empty, false *)
  (*         | BTrue  -> M.empty, true *)
  (*         | BIf(f,var,t) -> *)
  (*            let fmap,_ = aux f in *)
  (*            let tmap,_ = aux t in *)
  (*            let map = M.union (fun _ s1 s2 -> Some(List.append s1 s2)) fmap tmap in *)
  (*            M.add var ((id y,id f,id t)::(getM var map)) map,true *)
  (*       end *)
  (*   in *)
  (*   aux x *)

  let compile x = 
    let ident = ref 0 in
    let aux y =
      match y with
      | BFalse -> M.empty, false, -2
      | BTrue  -> M.empty, true, -1
      | BIf((fmap,_,fid),var,(tmap,_,tid)) ->
         let map = M.union (fun _ s1 s2 -> Some(List.append s1 s2)) fmap tmap in
         incr ident;
         M.add var ((!ident,fid,tid)::(getM var map)) map,true,!ident
    in
    foldb aux x

  open Format

  let ratio a b = (float_of_int a) /. (float_of_int b)
         
  let pp pp_node fmt t =
    let map,b,_ = compile t in
    let height = M.cardinal map in
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
    let _ = M.fold aux map 0 in
    let aux var l =
      let aux_level (i,j,k) =
        fprintf fmt "\\path[draw,dashed,->] (%i) -- node {} (%i);\n" i j;
        fprintf fmt "\\path[draw,->] (%i) -- node {} (%i);\n" i k
      in
      List.iter aux_level l
    in
    M.iter aux map;
    fprintf fmt "\\end{tikzpicture}"
    
  let to_string_aux a =
    let buf = Buffer.create 255 in
    let fmt = Format.formatter_of_buffer buf in
    a (Format.fprintf fmt);
    Format.fprintf fmt "%!";
    Buffer.contents buf
                    
  let to_string pp_node a = to_string_aux (fun p->p "%a" (pp pp_node) a)
      
end
              
let man = MLBDD.init()

let f = MLBDD.ithvar man 79
(* let g = BDD.reveal f *)
let () = print_endline(MLBDD.to_string f)
let _ = MLBDD.id f
let _ = match MLBDD.inspectb(f) with
  | MLBDD.BIf(g,i,d) -> MLBDD.inspectb g, MLBDD.inspectb d
  | _ -> failwith ""

let fmap,_,_ = BDD.compile f
let _ = BDD.M.cardinal fmap

exception NotConstantSig
  
module C : HardCaml.Transform.CombBaseGates = struct

  type t = MLBDD.t array

  let width = Array.length

  let const v = 
    let toBDD = function '0' -> MLBDD.dfalse man
                       | '1' -> MLBDD.dtrue man
                       | _   -> failwith "invalid constant" in
    let len = String.length v in
    let rec const b i = 
      if len = i then b 
      else const (toBDD v.[i] :: b) (i+1)
    in
    Array.of_list(List.rev (const [] 0))


  let empty : t = [||]
                    
  let select bv hi lo = Array.sub bv (width bv-hi-1) (hi-lo+1)

  let concat = Array.concat

  let wire len =
    let rec list b i = 
      if len = i then b 
      else list ((MLBDD.ithvar man (-i)) :: b) (i+1)
    in
    Array.of_list(list [] 0)

  let fold f0 f1 =
    Array.fold_right
      (fun bdd sofar ->
        if MLBDD.is_true bdd then f1 sofar
        else
          if MLBDD.is_false bdd then f0 sofar
          else raise NotConstantSig
      )
                 
  let to_int c 
    = fold (fun x->2*x) (fun x->2*1+1) c 0

  let to_bstr c 
    = fold (fun x->"0"^x) (fun x->"1"^x) c ""

  let to_string c =
    let pp_node fmt i =
      Format.fprintf fmt "$y[%i]$" (-i)
    in
    Array.fold_right
      (fun x sofar -> sofar^"\n"^(BDD.to_stringb x^"\n"^BDD.to_string pp_node x))
      c ""

  let (<==) bv1 bv2 : unit = failwith "Plugging wires is forbidden"
  let (--) a _ = a
  let (~:) = Array.map MLBDD.dnot 
  let (&:) = Array.map2 MLBDD.dand
  let (|:) = Array.map2 MLBDD.dor
  let (^:) = Array.map2 MLBDD.xor
end

module Circuit = HardCaml.Comb.Make(HardCaml.Transform.MakeCombGates(C))

open Circuit


let un = consti 4 1
let () = print_endline(to_bstr un)
let () = print_endline(to_bstr(select un 1 0))
                      
let deux = consti 4 2
let () = print_endline(to_bstr deux)

let var = wire 4
let dd = (((deux *+ var) +:. 1) &:. 1) ==:. 1
let () = print_endline(to_string dd)
                   
let dd = (((var) +:. 1) &:. 1) ==:. 1
let () = print_endline(to_string dd)

let smiii = (var <:. 3)
let () = print_endline(to_string smiii)

let odd = (select var 0 0 ==:. 1) &: (select var 0 0 ==:. 1)
let () = print_endline(to_string odd)
           
let () = print_endline(to_string(odd &: smiii))

let complex = sll (select (var *: var) 3 0) 2 <: (var -: (srl var 3))
let () = print_endline(to_string(complex))

let test = negate(((var *: var) >:. 4) &: (var <:. 10))
                      
let () = print_endline(to_string test)

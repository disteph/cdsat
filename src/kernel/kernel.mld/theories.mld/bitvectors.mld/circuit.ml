exception NotConstantSig
  
module C : HardCaml.Transform.CombBaseGates = struct

  type t = MLBDD.t array

  let width = Array.length

  let const v = 
    let toBDD = function '0' -> MLBDD.dfalse BDD.man
                       | '1' -> MLBDD.dtrue BDD.man
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
      else list ((MLBDD.ithvar BDD.man (-i)) :: b) (i+1)
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

(* open Circuit *)


(* let un = consti 4 1 *)
(* let () = print_endline(to_bstr un) *)
(* let () = print_endline(to_bstr(select un 1 0)) *)
                      
(* let deux = consti 4 2 *)
(* let () = print_endline(to_bstr deux) *)

(* let var = wire 4 *)
(* let dd = (((deux *+ var) +:. 1) &:. 1) ==:. 1 *)
(* let () = print_endline(to_string dd) *)
                   
(* let dd = (((var) +:. 1) &:. 1) ==:. 1 *)
(* let () = print_endline(to_string dd) *)

(* let smiii = (var <:. 3) *)
(* let () = print_endline(to_string smiii) *)

(* let odd = (select var 0 0 ==:. 1) &: (select var 0 0 ==:. 1) *)
(* let () = print_endline(to_string odd) *)
           
(* let () = print_endline(to_string(odd &: smiii)) *)

(* let complex = sll (select (var *: var) 3 0) 2 <: (var -: (srl var 3)) *)
(* let () = print_endline(to_string(complex)) *)

(* let test = negate(((var *: var) >:. 4) &: (var <:. 10)) *)
                      
(* let () = print_endline(to_string test) *)

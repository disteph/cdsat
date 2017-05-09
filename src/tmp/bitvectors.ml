open HardCaml.Bits.Comb.IntbitsList
let a = width (const "12'd0")

let man = MLBDD.init()

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
      else list ((MLBDD.ithvar man i) :: b) (i+1)
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
    Array.fold_right (fun x sofar -> sofar^"\n"^MLBDD.to_stringb x) c ""

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

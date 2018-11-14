exception NotConstantSig

type t = BDD.t array (* Most significant bit is the first cell of array *)

let width = Array.length
              
let const v = 
  let toBDD = function '0' -> BDD.dfalse
                     | '1' -> BDD.dtrue
                     | _   -> failwith "invalid constant"
  in
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
    else list ((BDD.ithvar (-i)) :: b) (i+1)
  in
  Array.of_list(list [] 0)

let fold f0 f1 =
  Array.fold_right
    (fun bdd sofar ->
      if BDD.is_true bdd then f1 sofar
      else
        if BDD.is_false bdd then f0 sofar
        else raise NotConstantSig)
    
let to_int c = fold (fun x->2*x) (fun x->2*1+1) c 0

let to_bstr c = fold (fun x->"0"^x) (fun x->"1"^x) c ""

let to_string c =
  let pp_node fmt i = Format.fprintf fmt "$y[%i]$" (-i) in
  Array.fold_right
    (fun x sofar -> sofar^"\n"^(BDD.to_stringb x^"\n"^BDD.to_string pp_node x))
    c ""

let (<==) bv1 bv2 : unit = failwith "Plugging wires is forbidden"
let (--) a _ = a
let (~:) = Array.map BDD.dnot 
let (&:) = Array.map2 BDD.dand
let (|:) = Array.map2 BDD.dor
let (^:) = Array.map2 BDD.xor

let bit i v = v.(Array.length v -i -1)

let isT a = (width a == 1)&&(BDD.is_true a.(0))

open Format
   
(* This is the module for constant bitvectors *)
module CstBV = HardCaml.Bits.Comb.ArraybitsNativeint
              
module V = struct
  include CstBV

  (* Predicate "is true ?" *)
  let isT a = [%eq:nativeint array * int] (a ==: vdd) vdd
  let equal a b = isT(a ==: b)
  let compare a b = if equal a b then 0
                    else if isT(a <=: b) then 1 else -1
  (* hash function only uses the lowest 16 bits: to be refined later? *)
  let hash a = CstBV.to_int(CstBV.select a 16 0) 
  let hash_fold_t = Hash.hash2fold hash
  let pp fmt a =
    let rec aux fmt = function
      | [] -> ()
      | t::q -> fprintf fmt "%s%a" (if t then "1" else "0") aux q
    in
    aux fmt (List.map isT (CstBV.bits a))
  let show = Format.stringOf pp
  let name = "BV"
end

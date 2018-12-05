open Top
       
module Known = struct
  let known =
    let open Symbols in
    function
    | BVextract _
    | BVconc _
    | BVcst _
    | BVnot _
    | BVneg _
    | BVand _     
    | BVor _
    | BVxor _
    | BVadd _
    | BVmul _
    | BVudiv _
    | BVurem _
    | BVshl _
    | BVshr _
    | BVult _
    | Eq (Sorts.BV _)
    | NEq (Sorts.BV _)
      -> true
    | _ -> false
  let name = "BV"
end

let key = Generic.make(module Known)

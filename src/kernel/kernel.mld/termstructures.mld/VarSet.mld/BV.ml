open Top
       
module Known = struct
  let known =
    let open Symbols in
    function
    | Conc _ | Extract _ | CstBV _
      | Eq (Sorts.BV _)
      | NEq (Sorts.BV _)
      -> true
    | _ -> false
end
             
           
include Generic.Make(Known)

open Top
       
module Known = struct
  let known =
    let open Symbols in
    function
    | Eq _ | NEq _ -> true
    | _ -> false
  let name = "Eq"
end
             
           
include Generic.Make(Known)

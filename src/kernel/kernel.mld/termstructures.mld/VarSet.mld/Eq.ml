open Top
       
module Known = struct
  let known =
    let open Symbols in
    function
    | Eq _
      | NEq _
      -> true
    | _ -> false
end
             
           
include Generic.Make(Known)

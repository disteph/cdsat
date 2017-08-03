open Top
       
module Known = struct
  let known =
    let open Symbols in
    function
    | ITE _ -> true
    | _ -> false
end
             
           
include Generic.Make(Known)

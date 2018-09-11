open Top
       
module Known = struct
  let known =
    let open Symbols in
    function
    | Eq _ | Select _ | Store _ | Diff _ -> true
    | _ -> false
  let name = "Arrays"
end
             
let key = Generic.make(module Known)

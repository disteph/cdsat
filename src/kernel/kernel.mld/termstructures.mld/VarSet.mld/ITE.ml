open Top
       
module Known = struct
  let known =
    let open Symbols in
    function
    | ITE _ -> true
    | _ -> false
  let name = "ITE"
end

let key = Generic.make(module Known)

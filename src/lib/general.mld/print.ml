(**********************)
(* Printing functions *)
(**********************)

module DTags = Map.Make(String)

let dtags : (int*bool)DTags.t ref = ref DTags.empty
                                        
let init idtags =
  dtags:=
    List.fold
      (fun (tag,level,b) -> DTags.add tag (level,b))
      idtags
      DTags.empty

(* Wait for key press *)
let wait () = Format.printf "%!";ignore (read_line ())

let print t msg =
  if DTags.is_empty !dtags then ()
  else
    let rec aux = function
      | [] -> ()
      | (tag,level)::tags
           when DTags.mem tag !dtags
        -> let i,b= DTags.find tag !dtags in
           if level <= i
           then (print_endline(Format.toString msg); if b then wait())
           else aux tags
      | _::tags -> aux tags
    in
    aux t

let stringOf = Format.stringOf

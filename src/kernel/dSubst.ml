(*************************)
(* Delayed Substitutions *)
(*************************)

open Format

exception Exception of string

type aux = EmptySubst | ConsSubst of World.FreeVar.t*World.t*t
and t    = {reveal : aux; id : int}

let id s = s.id

let equal s1 s2 = match s1, s2 with
  | EmptySubst, EmptySubst      -> true
  | ConsSubst(fv1,ar1,s1'), 
    ConsSubst(fv2,ar2,s2')
    -> (s1'==s2') && (fv1==fv2) && (World.equal ar1 ar2)
  | _,_ -> false

let hash s =
  match s with
  | EmptySubst -> 0
  | ConsSubst(fv,ar,s') -> 2 * (World.FreeVar.id fv) + 7 * id s'

module H = Hashtbl.Make(struct
  type t = aux
  let hash = hash
  let equal = equal
end)

let table = H.create 5003

let substid = ref 0

let build a =
  try H.find table a
  with Not_found ->
    let f = {reveal = a; id = !substid} in
    incr substid;
    H.add table a f;
    f

let print_in_fmt fmt t =
  match t.reveal with
  | EmptySubst -> ()
  | ConsSubst(fv,ar,s') ->
    let rec aux fmt t = match t.reveal with
      | EmptySubst -> failwith "Should not happen"
      | ConsSubst(fv,ar,s') ->
        begin match s'.reveal with
        | EmptySubst -> fprintf fmt "%a" World.FreeVar.print_in_fmt fv
        | _ -> fprintf fmt "%a;%a" World.FreeVar.print_in_fmt fv aux s'
        end
    in fprintf fmt "[%a]" aux t

let binit() = build EmptySubst

let compare s s' = Pervasives.compare (id s) (id s')
let clear () = substid := 0;  H.clear table; let _ = binit() in ()

let init = binit()
let bind2FV (fv,ar) l = build (ConsSubst(fv,ar,l))

let get_arity d = match d.reveal with
  | EmptySubst        -> World.init
  | ConsSubst(_,ar,_) -> ar

let rec get j d = match d.reveal with
  | EmptySubst -> raise (Exception 
                           (Dump.toString
                              (fun f -> f "Attempting to access bound variable %i in esubstitution %a" j print_in_fmt d)))
  | ConsSubst(fv,ar,d')
    -> if (j==0) then (fv,ar) else get (j-1) d'


(*******************************)
(* Standard Arities and DSubst *)
(*******************************)

open Format

open Kernel.Interfaces_I

(* Basic module for arities *)

module StandardArity = struct

  type eigen = int
  type meta  = int

  module IntMap = Map.Make(struct
    type t = int
    let compare = Pervasives.compare
  end)

  type t = {
    next_eigen : int;
    next_meta  : int;
    eigen_dependencies : int IntMap.t;
    meta_dependencies  : int IntMap.t
  }

  let init = {next_eigen = 0;
              next_meta  = 0; 
              eigen_dependencies = IntMap.empty;
              meta_dependencies  = IntMap.empty;
             }

  let liftE ar = {
    next_eigen = ar.next_eigen+1;
    next_meta  = ar.next_meta; 
    eigen_dependencies = IntMap.add ar.next_eigen ar.next_meta ar.eigen_dependencies;
    meta_dependencies  = ar.meta_dependencies;
  }

  let liftM ar = {
    next_eigen = ar.next_eigen;
    next_meta  = ar.next_meta+1; 
    eigen_dependencies = ar.eigen_dependencies;
    meta_dependencies  = IntMap.add ar.next_meta ar.next_eigen ar.meta_dependencies;
  }

  let projE ar = {
    next_eigen = ar.next_eigen-1;
    next_meta  = ar.next_meta; 
    eigen_dependencies = IntMap.remove (ar.next_eigen-1) ar.eigen_dependencies;
    meta_dependencies  = ar.meta_dependencies;
  }

  let projM ar = {
    next_eigen = ar.next_eigen;
    next_meta  = ar.next_meta-1; 
    eigen_dependencies = ar.eigen_dependencies;
    meta_dependencies  = IntMap.remove (ar.next_meta-1) ar.meta_dependencies;
  }


  let newEigen ar = ar.next_eigen,(liftE ar)
  let newMeta ar  = ar.next_meta,(liftM ar)

  let equal a1 a2 =
    (a1.next_eigen == a2.next_eigen)
    && (a1.next_meta == a2.next_meta)
    && (IntMap.equal (=) a1.eigen_dependencies a2.eigen_dependencies)
    && (IntMap.equal (=) a1.meta_dependencies a2.meta_dependencies)

  let prefix a1 a2 =
    (a1.next_eigen <= a2.next_eigen)
    && (a1.next_meta <= a2.next_meta)
    && (IntMap.for_all (fun ei nbm -> nbm == IntMap.find ei a2.eigen_dependencies) a1.eigen_dependencies)
    && (IntMap.for_all (fun mv nbe -> nbe == IntMap.find mv a2.meta_dependencies) a1.meta_dependencies)

  let print_in_fmtEM fmt ar = 
    let aux fmt =
      IntMap.fold (fun eigen mv () -> Format.fprintf fmt "%i -> #%i; " eigen mv) ar.eigen_dependencies ()
    in
    Format.fprintf fmt "%i; %t" ar.next_meta aux 

  let print_in_fmtME fmt ar = 
    let aux fmt =
      IntMap.fold (fun mv eigen () -> Format.fprintf fmt "%i -> #%i; " mv eigen) ar.meta_dependencies ()
    in
    Format.fprintf fmt "%i; %t" ar.next_meta aux 

  let print_in_fmt = print_in_fmtEM

end

(* Basic module for delayed substitutions *)

module StandardDSubst = struct

  exception DSubstException of string

  module Arity = StandardArity

  type aux = EmptySubst | ConsE of Arity.eigen*t | ConsM of Arity.meta*t
  and t = {reveal : aux; ar: Arity.t; id : int}

  let reveal s = s.reveal
  let id s = s.id
  let get_arity s = s.ar

  let equal s1 s2 = match (reveal s1,reveal s2) with
    | EmptySubst, EmptySubst                   -> true
    | ConsE(a,s1'), ConsE(b,s2') -> (s1'==s2') && (a == b)
    | ConsM(a,s1'), ConsM(b,s2') -> (s1'==s2') && (a == b)
    | _,_ -> false

  let hash s =
    match s.reveal with
    | EmptySubst -> 0
    | ConsE(a,s') -> 2 * a + id s'
    | ConsM(a,s') -> 3 * a + id s'

  module H = Hashtbl.Make(struct
    type t1 = t
    type t = t1
    let hash = hash
    let equal = equal
  end)

  let table = H.create 5003

  let substid = ref 0

  let build a ar =
    let f = {reveal = a; ar = ar; id = !substid} in
    try H.find table f
    with Not_found ->
      incr substid;
      H.add table f f;
      f

  let print_in_fmt fmt t =
    let rec aux fmt t = match t.reveal with
    | EmptySubst -> fprintf fmt ""
    | ConsE(a,s') -> (match s'.reveal with
      | EmptySubst -> fprintf fmt "%i" a
      | _ -> fprintf fmt "%i;%a" a aux s')
    | ConsM(a,s') -> (match s'.reveal with
      | EmptySubst -> fprintf fmt "?%i" a
      | _ -> fprintf fmt "?%i;%a" a aux s')
    in fprintf fmt "[%a]" aux t

  let binit() = build EmptySubst Arity.init

  let compare s s' = Pervasives.compare (id s) (id s')
  let clear () = substid := 0;  H.clear table; let _ = binit() in ()

  let init = binit()
  let bind2eigen (e,ar) l = build (ConsE(e,l)) ar
  let bind2meta (e,ar) l  = build (ConsM(e,l)) ar

  type freeVar = Eigen of Arity.eigen | Meta of Arity.meta

  let get i d =
    let rec aux j d = match reveal d with
    | EmptySubst -> raise (DSubstException 
      (Dump.toString
         (fun f -> f "Attempting to access bound variable %i in esubstitution %a" j print_in_fmt d)))
    | ConsE(k,d') -> if (j==0) then  Eigen k else aux (j-1) d'
    | ConsM(k,d') -> if (j==0) then  Meta k else aux (j-1) d'
    in
    aux i d
end

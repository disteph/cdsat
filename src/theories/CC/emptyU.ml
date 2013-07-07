open MyAtom
open EmptyCC
open MyPUF

(* the UnionFind module for CC(Empty) *)

module Ord = struct
  type t = int
  let compare = Pervasives.compare
end

module M = Map.Make (Ord)

type e = t

(* we keep the equalities on the arcs to get the explainations later *)
type d = input

type t = {classes : d PUnionFind.t; tmap : e M.t}

let create = {classes = PUnionFind.create; tmap = M.empty}

let add t e = {classes = PUnionFind.add t.classes (Term.id e); 
	       tmap = M.add (Term.id e) e t.tmap}

(* addLink must:
   - add the third argument to the classes if it doesn't exist
   - do nothing if there is already an arc between the two nodes
   - add the arc with the label d and 
   make the nodes the representatives of their class on the other case *)
let addLink t e e' d = 
  try
    let _ = M.find (Term.id e') t.tmap in
    {classes = PUnionFind.addLink t.classes (Term.id e) (Term.id e') (Some d);
     tmap = t.tmap}
  with
    Not_found -> 
      {classes = PUnionFind.addLink t.classes (Term.id e) (Term.id e') (Some d);
       tmap = M.add (Term.id e') e' t.tmap}

let find t e = M.find (PUnionFind.find t.classes (Term.id e)) t.tmap

let union t e e' = 
  {classes = PUnionFind.union t.classes (Term.id e) (Term.id e');
   tmap = t.tmap}

let path t e = PUnionFind.path t.classes (Term.id e)

let pathTo t e e' = PUnionFind.pathTo t.classes (Term.id e) (Term.id e')

let fca t e e' = 
  M.find (PUnionFind.fca t.classes (Term.id e) (Term.id e')) t.tmap

let clear () = PUnionFind.clear ()

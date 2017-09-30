open Format

open General
open Kernel.Top
open Kernel.Theories.Bitvectors
       
type 'a t = BDD.t * 'a list

let pp fmt (t,_) = BDD.pp (fun fmt i -> fprintf fmt "%i" i) fmt t
let show r = Print.stringOf pp r
  
let init = BDD.dtrue, []

let pick _ = failwith "TODO"
let mem _ _ = failwith "TODO"

type 'a update =
  | Range of 'a t
  | Singleton of MyTheory.V.t
  | Empty of 'a list

let update _ _ _ = failwith "TODO"

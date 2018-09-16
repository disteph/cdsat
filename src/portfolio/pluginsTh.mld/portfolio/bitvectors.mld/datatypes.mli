open Format
       
open General.Patricia

open Kernel
open Top
open Terms
open Sassigns

module Domain : Map.S with type keys   = Term.t
                       and type values = Assign.t Range.t
                       and type common = int
                       and type branching = int

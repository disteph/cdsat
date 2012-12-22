open Kernel
open Formulae
open Collection

module type Type =
  functor (ASet: ACollectImplem)-> 
    (sig
       val consistency: ASet.t -> ASet.t option
       val goal_consistency: ASet.t -> Atom.t -> ASet.t option
     end)    

type _ t = 
| NoTheory: unit t

let id (type a) (NoTheory: a t) = 0

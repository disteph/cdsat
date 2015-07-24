type _ t = 
| Empty: Empty.Mytheory.sign t

let id (type a) (Empty: a t) = 0

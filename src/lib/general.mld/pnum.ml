type t      = Num.num
let equal   = Num.(=/)
let compare = Num.compare_num
let hash = Num.int_of_num (* Not 100% sure about whether this hash function is compatible with equality *)
let hash_fold_t = Hash.hash2fold hash
let pp fmt t = Format.fprintf fmt "%s" (Num.string_of_num t)
let show = Num.string_of_num

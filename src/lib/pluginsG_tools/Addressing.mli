type nodeType = AndNode | OrNode

type 'a addressing

exception AddressingError of string

val data_of_ad : 'a addressing -> 'a
val print_in_fmt_ad : Format.formatter -> 'a addressing -> unit
val ad_up      : 'a addressing -> 'b -> 'b addressing
val ad_init    : 'a -> bool list -> 'a addressing
val branch     : nodeType -> 'a addressing -> 'a addressing
val el_wrap    : 'a addressing -> 'b list -> 'a addressing (* makes sure second argument is the empty list *)
val and_branch : 'a addressing -> 'a -> 'a -> (bool list -> 'a addressing) * (bool list -> 'a addressing)
val branch_one : 'a addressing -> (bool list -> 'a addressing) * (bool list -> 'a addressing)
val branch_two : 'a addressing -> (bool list -> 'a addressing) * (bool list -> 'a addressing)

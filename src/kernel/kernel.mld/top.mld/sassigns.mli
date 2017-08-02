(**********************)
(* Single Assignments *)
(**********************)

type ('t,'v) sassign = SAssign : ('t * ('v,_) Values.t) -> ('t,'v) sassign
                                                             [@@unboxed]
                                                             [@@deriving eq,ord,hash,show]
val is_Boolean : (_,_) sassign -> bool
                                    
type ('t,'v) bassign = 't * ('v,bool) Values.t [@@deriving eq,ord,hash,show]
val negation : ('t,'v) bassign -> ('t,'v) bassign
val boolassign : ?b:bool -> 'term -> ('term,_) sassign

type 'v values = Values : ('v,_) Values.t -> 'v values
                                               [@@unboxed]
                                               [@@deriving eq,ord,hash,show]

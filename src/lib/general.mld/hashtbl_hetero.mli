include module type of Hashtbl_hetero_sig

module MakeS(K:Keys)(D:sig type ('a,'b) t end)
  : S with type 'a key = 'a K.t
        and type ('a,'b) data = ('a,'b) D.t

module MakeR(K:Keys) : R with type 'a key = 'a K.t
module MakeT(K:Keys) : T with type 'a key = 'a K.t

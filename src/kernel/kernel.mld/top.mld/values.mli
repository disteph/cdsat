(**********)
(* Values *)
(**********)

type nbool = private NBool
       
type ('v,_) t =
  | NonBoolean : 'v -> ('v,nbool) t
  | Boolean    : bool -> ('v,bool) t
                           [@@deriving eq,ord,show]

val hash_fold_t : ('v Ppx_hash_lib.Std.Hash.folder) ->
                (('v, _) t) Ppx_hash_lib.Std.Hash.folder

val hash_fold : ('b Ppx_hash_lib.Std.Hash.folder) ->
                ('c Ppx_hash_lib.Std.Hash.folder) ->
                ('b * ('c, _) t) Ppx_hash_lib.Std.Hash.folder

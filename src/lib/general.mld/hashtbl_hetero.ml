include Hashtbl_hetero_sig

module MakeS(K:Keys)(D:sig type ('a,'b) t end)
= struct
  module H = Hashtbl.Make(struct type t = int let equal = (=) let hash x = x end)

  type 'a key = 'a K.t
  type ('a,'b) data = ('a,'b) D.t

  type 'b elt = Pair : 'a key * ('a,'b) D.t -> 'b elt
  type 'b t = 'b elt H.t

  type ('b, 'c) fold = { fold : 'a. 'a key -> ('a, 'b) data -> 'c -> 'c }
  type printk    = { printk : 'a. 'a key Format.printer }
  type 'b printd = { printd : 'a. 'a key -> ('a, 'b) data Format.printer }

  module Includable = struct

    let mem (m: 'b t) (k : 'a key) : bool = H.mem m (K.hash k)

    let find (type a) (m: 'b t) (k : a key) : (a, 'b) data =
      let Pair(k',r) = H.find m (K.hash k) in
      match K.equal k k' with
      | Poly.Eq -> r
      | Poly.Neq -> raise IncoherentTable

    let add (m: 'b t) (k: 'a key) (v : ('a, 'b) data) : unit =
      H.replace m (K.hash k) (Pair(k,v))

    let remove (m: 'b t) (k : 'a key) : unit = H.remove m (K.hash k)

    let create capacity : 'b t = H.create capacity

    let clear : 'b t -> unit = H.clear

    let fold (f: ('b, 'c) fold) (m : 'b t) (seed : 'c) : 'c =
      H.fold (fun _ (Pair(k,v)) sofar -> f.fold k v sofar) m seed

    let copy : 'b t -> 'b t = H.copy

    let pp (sep1 : string) (sep2 : string)
        (printkey : printk) (printdata : 'b printd) : 'b t Format.printer
      =
      fun fmt t ->
        let printkeydata fmt (Pair(k,v)) =
          Format.fprintf fmt "%a%s%a" printkey.printk k sep2 (printdata.printd k) v
        in
        let as_list = H.fold (fun _ v sofar -> v::sofar) t [] in
        List.pp ~sep:sep1 printkeydata fmt as_list
  end

  include Includable
end

module MakeR(K:Keys) : R with type 'a key = 'a K.t = struct

  include MakeS(K)(struct type (_,'b) t = 'b end)

  let fold f m seed = H.fold (fun _ (Pair(_,v)) sofar -> f v sofar) m seed

  let pp (type b) (sep1 : string) (sep2 : string)
      (printkey : printk) (printdata : b Format.printer) : b t Format.printer
    =
    fun fmt t ->
      let printkeydata fmt (Pair(k,v)) =
        Format.fprintf fmt "%a%s%a" printkey.printk k sep2 printdata v
      in
      let as_list = H.fold (fun _ v sofar -> v::sofar) t [] in
      List.pp ~sep:sep1 printkeydata fmt as_list

end

module MakeT(K:Keys) : T with type 'a key = 'a K.t = struct
  module S = MakeS(K)(struct type ('a,_) t = 'a end)
  type t = unit S.t
  type 'a key = 'a K.t
  type 'c fold = { fold : 'a. 'a key -> 'a -> 'c -> 'c; }
  type printk = { printk : 'a. 'a key Format.printer; }
  type printd = { printd : 'a. 'a key -> 'a Format.printer; }
  include S.Includable
  let fold {fold} = S.fold {fold}
  let pp (sep1 : string) (sep2 : string)
      (printkey : printk) (printdata : printd) : t Format.printer
    =
    fun fmt t ->
      let printkeydata fmt (S.Pair(k,v)) =
        Format.fprintf fmt "%a%s%a" printkey.printk k sep2 (printdata.printd k) v
      in
      let as_list = S.H.fold (fun _ v sofar -> v::sofar) t [] in
      List.pp ~sep:sep1 printkeydata fmt as_list
end

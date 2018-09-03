include Keys_sig

module Make() = struct

  type _ gadt = ..
  type 'a t = { gadt : 'a gadt;
                name : string;
                id   : int;
                iseq : 'b. 'b gadt -> ('a,'b) Poly.iseq }

  let pp fmt x = Stringhashed.pp fmt x.name
  let show k = Print.stringOf pp k
  let compare x y = compare x.id y.id
  let id   x = x.id
  let hash   = id
  let name x = x.name
                 
  let equal a b = a.iseq b.gadt
  let eq a b    = equal a b |> Poly.eq
  let coerce (type a) (type b) (a:a t) (b:b t) (x:a) : b = let Poly.Eq = eq a b in x

  type key = K : _ t -> key [@@unboxed]

  module AllKeys = Hashtbl.Make(struct
      type t = key
      let equal (K a) (K b) = a.id = b.id
      let hash (K a) = a.id
    end)

  let all_keys = AllKeys.create 17
  
  let make (type a) (module NT : NamedType with type t = a) : a t =
    let module TMP = struct
      type _ gadt += K : NT.t gadt
    end in
    let iseq : type b. b gadt -> (NT.t,b) Poly.iseq = function
      | TMP.K -> Poly.Eq
      | _ -> Poly.Neq
    in
    let key = { gadt = TMP.K;
                name = NT.name;
                id   = AllKeys.length all_keys;
                iseq }
    in
    AllKeys.replace all_keys (K key) ();
    key

  type 'b fold = {fold : 'a. 'a t -> 'b -> 'b} [@@unboxed]
  let fold f = AllKeys.fold (fun (K x) () -> f.fold x) all_keys

end

open Patricia

include Map_hetero_sig

type ('k,'v,'common,'branching,'hcons) poly
  = ('k,'v,'common,'branching,unit*'hcons) Patricia.poly

module Build(I:ArgNH) = struct

  module A = struct
    type t = K : _ I.t -> t
    let compare (K k1) (K k2) = match I.compare k1 k2 with
      | Poly.Eq -> 0
      | Poly.Gt -> -1
      | Poly.Lt -> 1

    type common = I.common
    let tag (K k) = I.tag k

    type branching   = I.branching
    let bcompare     = I.bcompare
    let check        = I.check
    let disagree     = I.disagree
    let match_prefix = I.match_prefix

    type values = Pair : ('a I.t * 'a I.values) -> values [@@unboxed]
    include Patricia_tools.EmptyInfo
  end

  module From(P: Map.S with type keys   = A.t
                        and type values = A.values
                        and type common = I.common
                        and type branching = I.branching
                        and type infos = Patricia_tools.EmptyInfo.infos)
  = struct

    type t         = P.t
    type hcons     = P.hcons
    type 'a keys   = 'a I.t
    type 'a values = 'a I.values
    type common    = I.common
    type branching = I.branching

    let is_empty = P.is_empty
    let cardinal = P.cardinal
    let empty    = P.empty

    let mem k    = P.mem (K k)

    let find (type a) (k : a I.t) t =
      let Pair(k',v) = P.find (K k) t in
      let Poly.Eq = I.compare k k' |> Poly.eq in (v: a I.values)

    let singleton k v = P.singleton (K k) (Pair(k,v))

    let remove k t = P.remove (K k) t

    let add (type a) (k : a I.t) f t =
      let aux = function
        | None -> A.Pair(k,f None)
        | Some(A.Pair(k',v)) ->
          let Poly.Eq = I.compare k k' |> Poly.eq in A.Pair(k,f(Some (v:a I.values)))
      in
      P.add (K k) aux t

    type ('seed,'b) fold = { fold : 'p. 'p keys -> 'p values -> 'seed -> 'b} [@@unboxed]

    let fold_aux {fold} (A.K k) (A.Pair(k',v)) =
      let Poly.Eq = I.compare k k' |> Poly.eq in
      fold k v

    let fold f = f |> fold_aux |> P.fold

    let fold_monad ~return ~bind f = f |> fold_aux |> P.fold_monad ~return ~bind

    module Fold2 = struct

      type 'b combine = {
        combineFF : t -> t -> 'b -> 'b;
        combineEF : t -> 'b -> 'b;
        combineFE : t -> 'b -> 'b }

      let combine_from P.Fold2.{combineFF; combineEF; combineFE}
        = {combineFF; combineEF; combineFE}
      let combine_to {combineFF; combineEF; combineFE}
        = P.Fold2.{combineFF; combineEF; combineFE}

      let make_combine combine ~reccall =
        combine_from(P.Fold2.make_combine ~empty1:P.empty ~empty2:P.empty
                       combine ~reccall)

      type nonrec ('a,'b) t = {
        sameleaf  : 'p. 'p keys -> 'p values -> 'p values -> 'a -> 'b;
        emptyfull : t -> 'a -> 'b;
        fullempty : t -> 'a -> 'b;
        combine   : reccall : (t -> t -> 'a -> 'b) -> 'b combine
      }

      let t_from P.Fold2.{sameleaf; emptyfull; fullempty; combine} =
        let sameleaf k v1 v2 a =
          sameleaf (A.K k) (A.Pair(k,v1)) (A.Pair(k,v2)) a
        in
        {sameleaf; emptyfull; fullempty;
         combine = fun ~reccall -> combine ~reccall |> combine_from}

      let t_to {sameleaf; emptyfull; fullempty; combine}
        =
        let sameleaf (A.K k) (A.Pair(k1,v1)) (A.Pair(k2,v2)) a =
          let Poly.Eq = I.compare k k1 |> Poly.eq in
          let Poly.Eq = I.compare k k2 |> Poly.eq in
          sameleaf k v1 v2 a
        in
        P.Fold2.{sameleaf; emptyfull; fullempty;
                 combine = fun ~reccall -> combine ~reccall |> combine_to}

    end

    let fold2 ?equal fold = P.fold2 ?equal (Fold2.t_to fold)

    module Merge = struct
      type nonrec 'a t = {
        sameleaf  : 'p. 'p keys -> 'p values -> 'p values -> 'a;
        emptyfull : t -> 'a;
        fullempty : t -> 'a;
        combine   : 'a -> 'a -> 'a
      }

      let t_from P.Merge.{sameleaf; emptyfull; fullempty; combine} =
        let sameleaf k v1 v2 =
          sameleaf (A.K k) (A.Pair(k,v1)) (A.Pair(k,v2))
        in
        {sameleaf; emptyfull; fullempty; combine}

      let t_to {sameleaf; emptyfull; fullempty; combine}
        =
        let sameleaf (A.K k) (A.Pair(k1,v1)) (A.Pair(k2,v2)) =
          let Poly.Eq = I.compare k k1 |> Poly.eq in
          let Poly.Eq = I.compare k k2 |> Poly.eq in
          sameleaf k v1 v2
        in
        P.Merge.{sameleaf; emptyfull; fullempty; combine}
    end

    let merge ?equal fold = P.merge ?equal (Merge.t_to fold)

    type union = {union : 'p. 'p keys -> 'p values -> 'p values -> 'p values} [@@unboxed]

    let union {union} =
      let aux (A.K k) (A.Pair(k1,v1)) (A.Pair(k2,v2)) =
        let Poly.Eq = I.compare k k1 |> Poly.eq in
        let Poly.Eq = I.compare k k2 |> Poly.eq in
        A.Pair(k,union k v1 v2)
      in
      P.union aux

    type inter = {inter : 'p. 'p keys -> 'p values -> 'p values -> 'p values} [@@unboxed]

    let inter {inter} =
      let aux (A.K k) (A.Pair(k1,v1)) (A.Pair(k2,v2)) =
        let Poly.Eq = I.compare k k1 |> Poly.eq in
        let Poly.Eq = I.compare k k2 |> Poly.eq in
        A.Pair(k,inter k v1 v2)
      in
      P.inter aux

    type diff  = {diff : 'p. 'p keys -> 'p values -> 'p values -> t} [@@unboxed]

    let diff {diff} =
      let aux (A.K k) (A.Pair(k1,v1)) (A.Pair(k2,v2)) =
        let Poly.Eq = I.compare k k1 |> Poly.eq in
        let Poly.Eq = I.compare k k2 |> Poly.eq in
        diff k v1 v2
      in
      P.diff aux

    type subset = {subset : 'p. 'p values -> 'p values -> bool} [@@unboxed]

    let subset {subset} =
      let aux (A.Pair(k1,v1)) (A.Pair(k2,v2)) =
        let Poly.Eq = I.compare k1 k2 |> Poly.eq in
        subset v1 v2
      in
      P.subset aux

    type print_pair = {print_pair: 'p . ('p keys * 'p values) Format.printer} [@@unboxed]

    let print_in_fmt ?tree ?sep ?wrap {print_pair} =
      let aux fmt (_,(A.Pair p)) = print_pair fmt p in
      P.print_in_fmt ?tree ?sep ?wrap aux
  end

end

module MakeNH(I:ArgNH) = struct

  include Build(I)
  include From(Map.MakeNH(A))

end

module MakeH(I:ArgH) = struct

  include Build(I)
  module AH = struct
    include A
    let hash_fold_t state (K k) = I.hash_fold_t state k
    let hash_fold_values state (Pair(k,v))
      = Hash.pair I.hash_fold_t (I.hash_fold_values k) state (k,v)
    let equal_values (Pair(k1,v1)) (Pair(k2,v2)) = match I.compare k1 k2 |> Poly.iseq with
      | Poly.Eq  -> I.equal_values k1 v1 v2
      | Poly.Neq -> false
  end
  module P = Map.MakeH(AH)
  include From(P)
  let clear       = P.clear
  let id          = P.id
  let compare     = P.compare
  let hash        = P.hash
  let hash_fold_t = P.hash_fold_t
  let equal       = P.equal
end

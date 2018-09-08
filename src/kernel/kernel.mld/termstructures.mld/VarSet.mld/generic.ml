open Top
open Basic
open Terms

module Make(S: sig
    val known : Symbols.t -> bool
    val name  : string
  end) = struct

  type t = TSet.t [@@deriving show]

  let key = ThTermKey.make(module struct type nonrec t = t let name = S.name end)

  let build t =
    match Term.reveal t with
    | C(symb,l) when S.known symb
      -> List.fold (proj key >> TSet.union) l TSet.empty
    | _ -> TSet.singleton t

end

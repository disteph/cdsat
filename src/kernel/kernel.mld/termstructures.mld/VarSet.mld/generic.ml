open Top
open Basic
open Terms

module type Arg = sig
  (* val ksorts : Sorts.t -> bool *)
  val known : Top.Symbols.t -> bool
  val name  : string
end

let make (module S : Arg) =
  Terms.Key.make(module struct
    type t = TSet.t [@@deriving show]

    let build ~reccall t =
      match Term.reveal t with
      | C(symb,l) when S.known symb
        -> List.fold (reccall >> TSet.union) l TSet.empty
      | _ -> TSet.singleton t

    let name = S.name
  end)

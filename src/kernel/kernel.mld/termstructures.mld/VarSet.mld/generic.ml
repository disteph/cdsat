open Top
open Interfaces_basic
open Specs

module Make(S: sig
    val known : Symbols.t -> bool
  end) = struct

  include Termstructure.Make(struct
      type (_,'tset) t = 'tset

      module Make(Term : Term)(TSet : Collection with type e = Term.t) = struct

        type t = TSet.t [@@deriving show]

        let build ~proj (t:Term.t) : t =
          match Terms.reveal t with
          | Terms.C(symb,l) when S.known symb
            -> List.fold (fun t -> (t |> Terms.data |> proj |> TSet.union)) l TSet.empty
          | _ -> TSet.singleton t

      end
    end)

end

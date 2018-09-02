open Top
open Basic
open Specs

module Make(S: sig
    val known : Symbols.t -> bool
  end) = struct

  include Termstructure.Make(struct
      type (_,'tset) t = 'tset
      type (_,_) api = unit

      module Make(Term : Term)(TSet : Collection with type e = Term.t) = struct

        type t = TSet.t [@@deriving show]

        let build ~proj t =
          match Terms.reveal t with
          | Terms.C(symb,l) when S.known symb
            -> List.fold (Terms.data >> proj >> TSet.union) l TSet.empty
          | _ -> TSet.singleton t

        let api = ()
                  
      end
    end)

end

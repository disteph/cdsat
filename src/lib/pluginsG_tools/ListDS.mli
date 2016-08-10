(* ******************************************* *)
(* Implementation of sets of atoms with Lists,
   Implementation of formulae info with Lists,
   Implementation of sets of formulae with Lists *)
(* ******************************************* *)
open Format
open Kernel.Prop
       
module Generate : sig

  module UF : Formulae.FormulaF.Extra

  module UFSet : sig
    include (Interfaces_plugin.CollectExtra with type e = UF.t Formulae.FormulaF.generic)
    val fold: (e -> 'a -> 'a) -> t -> 'a -> 'a
    val choose: t -> e option
    val print_in_fmt : ?print_atom:(formatter -> int -> unit) -> Format.formatter -> t -> unit
  end

  module UASet : sig
    include (Interfaces_plugin.CollectExtra with type e = Literals.LitF.t)
    val fold: (e -> 'a -> 'a) -> t -> 'a -> 'a
    val print_in_fmt : ?print_atom:(formatter -> int -> unit) -> Format.formatter -> t -> unit
  end

end
  

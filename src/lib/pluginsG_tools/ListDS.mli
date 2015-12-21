(* ******************************************* *)
(* Implementation of sets of atoms with Lists,
   Implementation of formulae info with Lists,
   Implementation of sets of formulae with Lists *)
(* ******************************************* *)

module Generate : sig

  module UF : Kernel.Prop.Formulae.FormulaF.Extra

  module UFSet : sig
    include (Kernel.Prop.Interfaces_plugin.CollectExtra 
             with type e = UF.t Kernel.Prop.Formulae.FormulaF.generic)
    val fold: (e -> 'a -> 'a) -> t -> 'a -> 'a
    val choose: t -> e option
    val print_in_fmt : Format.formatter -> t -> unit
  end

  module UASet : sig
    include (Kernel.Prop.Interfaces_plugin.CollectExtra 
             with type e = Kernel.Prop.Literals.LitF.t)
    val fold: (e -> 'a -> 'a) -> t -> 'a -> 'a
    val print_in_fmt : Format.formatter -> t -> unit
  end

end
  

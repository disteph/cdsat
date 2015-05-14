(******************)
(* Theory Manager *)
(******************)

open Format

open Kernel
open Basic
open Interfaces_basic

module IJMon = (struct
  type 'a t = 'a*int*int
  let return a = (a,0,-1)
  let bind (f: 'a -> 'b t) (a,i,j)
      = let (b,i',j') = f a in
        (b,Pervasives.max i i',Pervasives.min j j')
end : MonadType with type 'a t = 'a*int*int)

module IdMon = (struct
  type 'a t = 'a
  let return a = a
  let bind (f: 'a -> 'b t) a = f a
end : MonadType with type 'a t = 'a)

module type Type = sig

  (* Implem of atoms and consistency checks, as required by kernel *)
  include Kernel.Interfaces_theory.DecProc

  val names    : string list
  val sugPlugin: string option

  module ForParsing(F: Kernel.Formulae.FormulaType with type lit = DS.Atom.t)
    :sig        
      include Theory.ForParsingType with type leaf = Kernel.Basic.IntSort.t
      val toForm : t -> F.t

      (* A list of illustrative examples *)
      val examples : ((unit->F.t)*bool) list
  end
end


module Make(MDP:Theory.Type): Type = struct

  let names     = MDP.names
  let sugPlugin = MDP.sugPlugin

  module DS = struct

    include MDP

    module IAtom = struct
      include Atom
      let proj i = i
    end

    let iatom_build (a,d) =
      let module M = Atom.Homo(IdMon) in
      let get_ij iso = 
        let (k,_) = IntSort.reveal iso in
        let (fv,ar) = Kernel.DSubst.get k d in
        (World.asIntSort fv)
      in
      M.lift get_ij a

    let makes_sense _ ar = true

    module Cons   = Consistency(IAtom)
    module ThASet = Cons.ASet
  end

  open DS

  let consistency = Cons.consistency
  let goal_consistency = Cons.goal_consistency

  module ForParsing = ForParsing

end

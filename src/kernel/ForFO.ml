(******************)
(* Theory Manager *)
(******************)

open Format

open Basic
open Interfaces_basic

module IJMon = (struct
  type 'a t = 'a*int*int
  let return a = (a,0,-1)
  let bind (f: 'a -> 'b t) (a,i,j)
      = let (b,i',j') = f a in
        (b,Pervasives.max i i',Pervasives.min j j')
end : MonadType with type 'a t = 'a*int*int)


module type S = sig

  (* Implem of atoms and consistency checks, as required by kernel *)
  include Interfaces_theory.DecProc

  val names    : string list
  val sugPlugin: string option

  module ForParsing(F: Formulae.Formula.S with type lit = DS.Atom.t)
    :sig
      include OpenModule.ForParsingType with type leaf := IntSort.t
      val toForm : t -> F.t

      (* A list of illustrative examples *)
      val examples : ((unit->F.t)*bool) list
  end
end


module Make(MDP:Theory.Type): S = struct

  let names     = MDP.names
  let sugPlugin = MDP.sugPlugin

  module DS = struct

    include MDP

    module IAtom = struct

      type t = (Atom.t IJMon.t)*(Atom.t IJMon.t)
      let proj ((a,_,_),_)   = a
      let negation (a,b)     = (b,a)

      module Homo(Mon:MonadType) = struct
        module AHomo = Atom.Homo(Mon)
        let lift f ((a,i,j),(b,k,l)) =
          let aux c d = Mon.return ((c,i,j),(d,k,l)) in
          let aux2 c = Mon.bind (aux c) (AHomo.lift f b) in
          Mon.bind aux2 (AHomo.lift f a)
      end

      let id a               = Atom.id (proj a)
      let print_in_fmt fmt a = Atom.print_in_fmt fmt (proj a)
      let clear              = Atom.clear
      let compare a1 a2      = Atom.compare (proj a1) (proj a2)
    end

    let iatom_build (a,d) =
      let module M = Atom.Homo(IJMon) in
      let get_ij iso = 
        let k,_   = IntSort.reveal iso in
        let fv,ar = Prop.DSubst.get k d in
        (World.asIntSort fv, ar.World.next_eigen, ar.World.next_meta)
      in
      M.lift get_ij a,
      M.lift get_ij (Atom.negation a)

    let makes_sense ((_,i,j),_) ar = 
      (i <= ar.World.next_eigen) && (j >= ar.World.next_meta)

    module Cons   = Consistency(IAtom)
    module ThASet = Cons.ASet
  end

  open DS

  let consistency = Cons.consistency
  let goal_consistency = Cons.goal_consistency

  module ForParsing = ForParsing

end

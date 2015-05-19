open Format

open Kernel
open Interfaces_theory
open Basic
open Formulae
open Theories_tools.StandardStruct

let names     = ["FO"; "UF"]
let sugPlugin = None

include StandardDS(IntSort)

module ForParsing = ForParsingWOEx

include Unification.Make(Atom.Term)

module Consistency(EAtom : sig
  type t
  val proj: t -> Atom.t
  val negation: t->t
end) =
struct    

  module ASet = Sequents.MakeCollectTrusted(
    struct
      type t = EAtom.t
      let id t = Atom.id(EAtom.proj t)
      let clear = Atom.clear
      let compare a b = Atom.compare (EAtom.proj a) (EAtom.proj b)
      let print_in_fmt fmt a = Atom.print_in_fmt fmt (EAtom.proj a)
    end)

  exception FoundIt of ASet.t*Constraint.t*(ASet.t,Constraint.t) stream

  (* We range over atomN,
     alias is the set of atoms that haven't been tried (starts with atomN);
     as soon as we find a solution, we raise an exception *)

  let aux aset compare cont t atomN sigma = 
    ASet.fold
      (fun a alias -> 
        Dump.msg(Some(fun p -> p "Unifying atoms %a and %a" Atom.print_in_fmt (EAtom.proj a) Atom.print_in_fmt t))None None;
        let newalias = ASet.remove a alias in
        let (b1,p1,l1) = Atom.reveal (EAtom.proj a) in
        let (b2,p2,l2) = Atom.reveal t in
        (if (compare b1 b2) && (p1 = p2)
         then
            (Dump.msg (Some(fun p -> p "constraint = %a" Constraint.print_in_fmt sigma))None None;
             let internalise = List.map(IU.internalise (MKcorr.get_key sigma.Constraint.mk)) in
             match Constraint.unif sigma (internalise l1) (internalise l2) with
             | Some newsigma -> 
               raise (FoundIt(ASet.add a aset,newsigma, cont newalias))
             | None -> ()));
        newalias)
      atomN 
      atomN 
      
  let rec goal_consistency t atomN sigma = 
    (* (print_endline (Dump.toString(fun p->p "goal consistency on %a |- %a" ASet.print_in_fmt atomN))); *)
    (* We range over atomN,
       we catch an exception (means success);
       otherwise we finish with NoMore *)
    try 
      let _ = 
        aux ASet.empty (=) (goal_consistency t) (EAtom.proj t) atomN sigma
      in NoMore
    with
      FoundIt(a,newsigma,f) -> Guard(a,newsigma,f)

  let rec consistency atomN sigma =
    try 
      let _ =
        ASet.fold
          (fun t after -> 
            let newafter = ASet.remove t after in
            let aset = ASet.add t ASet.empty in
            let rec g_consistency remaining sigma =
              try 
                let _ = aux aset (!=) g_consistency (EAtom.proj t) remaining sigma in
                consistency newafter sigma
              with
                FoundIt(a,newsigma,f) -> Guard(a,newsigma,f)
            in
            let _ = aux aset (!=) g_consistency (EAtom.proj t) newafter sigma in newafter
          )
          atomN 
          atomN 
      in
      NoMore
    with
      FoundIt(a,newsigma,f) -> Guard(a,newsigma,f)

end


open General
open Patricia
open Kernel.Top.Messages


module type Config = sig

  include TwoWatchedLits.Config

  val howmany: int
  type stop
  type msg

  val init_fixed : fixed

  type result =
    | UNSAT     of stop
    | Propagate of fixed * Var.t list
    | Meh       of fixed

  val constreat  : Constraint.t -> fixed -> result

  val extract_msg: fixed -> (msg * fixed) option

end

module Make(C: Config) = struct

  module WL = TwoWatchedLits.Make(C)

  type t = {
    fixed : C.fixed;
    watch : WL.t
  }

  let init = {
    fixed = C.init_fixed;
    watch = WL.init
  }

  let rec propagate c t = 
    match C.constreat c t.fixed with
    | C.UNSAT stop -> Sums.Case1 stop
    | C.Propagate(fixed',varlist) -> fix fixed' varlist t
    | C.Meh fixed' ->
       run { t with fixed = fixed' }

  and run t = 
    let res, watch' = WL.next t.fixed ~howmany:C.howmany t.watch in
    let t = { t with watch = watch' } in
    match res with
    | Some(c,_) -> propagate c t
    | None   -> Sums.Case2 t

  and fix fixed varlist t =
    run {
        fixed = fixed;
        watch = List.fold_left (fun a b -> WL.fix b a) t.watch varlist
      }
                           
  let treat c t =
    run { t with watch = WL.addconstraintNflag c [] t.watch }
        
  let extract_msg t =
    match C.extract_msg t.fixed with
    | None -> None, t
    | Some(msg,fixed') -> Some msg,{ t with fixed = fixed' }


end

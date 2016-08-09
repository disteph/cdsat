open General
open Patricia
open Kernel.Top.Messages


module type Config = sig

  include TwoWatchedLits.Config

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

  let rec propagate ?howmany c t = 
    match C.constreat c t.fixed with
    | C.UNSAT stop -> Sums.A stop
    | C.Propagate(fixed',varlist) ->
       run ?howmany {
           fixed = fixed';
           watch = List.fold_left (fun a b -> WL.fix b a) t.watch varlist
         }
    | C.Meh fixed' ->
       run ?howmany { t with fixed = fixed' }

  and run ?howmany t = 
    let res, watch' = WL.next t.fixed ?howmany t.watch in
    let t = { t with watch = watch' } in
    match res with
    | Some(c,_) -> propagate ?howmany c t
    | None   -> Sums.F t

  let treat c howmany t =
    run ~howmany:howmany { t with watch = WL.addconstraintNflag c [] t.watch }

  let extract_msg t =
    match C.extract_msg t.fixed with
    | None -> None, t
    | Some(msg,fixed') -> Some msg,{ t with fixed = fixed' }


end

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
    | Watch     of fixed * Var.t * Var.t

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

  let rec treat_simplified c t = 
    match C.constreat c t.fixed with
    | C.Watch(fixed',var1,var2) ->
       run {
           fixed = fixed';
           watch = WL.addconstraint c var1 var2 t.watch
         }
    | C.UNSAT stop -> Sums.A stop
    | C.Propagate(fixed',varlist) ->
       run {
           fixed = fixed';
           watch = List.fold_left (fun a b -> WL.fix b a) t.watch varlist
         }
    | C.Meh fixed' ->
       run { t with fixed = fixed' }

  and run t = 
    let res, watch' = WL.next t.fixed t.watch in
    let t = { t with watch = watch' } in
    match res with
    | Some(c,_) -> treat_simplified c t
    | None   -> Sums.F t

  let treat c t =
    let c = C.simplify t.fixed c in
    treat_simplified c t

  let extract_msg t =
    match C.extract_msg t.fixed with
    | None -> None, t
    | Some(msg,fixed') -> Some msg,{ t with fixed = fixed' }


end
